#' Decision of a result in words
#'
#' @param result An `stv_result`.
#'
#' @return A string.
#' @noRd
stv_decision_text <- function(result) {
  if (identical(result@family, "tost")) {
    if (isTRUE(result@significant)) "Claim shown at this level" else "Claim not shown at this level"
  } else if (isTRUE(result@significant)) {
    sprintf("Significant at %s", format(result@alpha))
  } else {
    sprintf("Not significant at %s", format(result@alpha))
  }
}

#' Estimate and interval of a result in words
#'
#' @param result An `stv_result`.
#'
#' @return A string, or `NULL` when stabilitest returned no estimate.
#' @noRd
stv_estimate_text <- function(result) {
  if (!length(result@estimate) || !is.finite(result@estimate)) {
    return(NULL)
  }
  paste0(
    format(signif(result@estimate, 4)),
    if (length(result@ci) == 2L && all(is.finite(result@ci))) {
      paste0(" (", paste(format(signif(result@ci, 4), trim = TRUE), collapse = " to "), ")")
    }
  )
}

#' The removal step at which the decision flips, in words
#'
#' @param result An `stv_result`.
#'
#' @return A string.
#' @noRd
stv_fragility_text <- function(result) {
  m <- result@metrics
  k <- m$worstcase_fragility_k
  if (length(k) && is.finite(k)) {
    sprintf(
      "Decision flips after removing %s obs (%s%%)",
      stv_format_number(k, 0), stv_format_number(m$worstcase_fragility_pct)
    )
  } else {
    "Decision did not flip within the removal limit"
  }
}

#' Label of a result, or the fail-closed text
#'
#' @param result An `stv_result`.
#'
#' @return A string.
#' @noRd
stv_label_text <- function(result) {
  if (is.na(result@label)) "None (numeric only, no validated cutoff)" else result@label
}

#' Summary of a result
#'
#' The numbers shown for a result outside the Analyse page: in Compare, the
#' exports and the report. Every value is one stabilitest returned, formatted.
#'
#' @param result An `stv_result`.
#'
#' @return A data frame with `measure` and `value` columns.
#' @noRd
stv_result_summary <- function(result) {
  m <- result@metrics
  w <- result@weights
  weight <- \(nm) if (nm %in% names(w)) paste0(" (weight ", round(100 * w[[nm]]), "%)") else ""
  rows <- c(
    "Method" = result@method_label,
    "n" = format(result@n),
    "Estimate" = stv_estimate_text(result) %||% "Not returned",
    "Original p-value" = stv_format_p(result@p_value),
    "Decision" = stv_decision_text(result),
    "Composite score" = paste0(stv_format_number(result@score, 0), " / 100"),
    "Jackknife stability" = paste0(stv_format_number(m$jackknife_conclusion_stability, 0), "%", weight("jackknife")),
    "Removal fragility" = paste0(stv_format_number(m$worstcase_fragility_component, 0), " / 100", weight("fragility")),
    "Removal flip" = stv_fragility_text(result),
    "Bootstrap same-decision rate" = paste0(stv_format_number(m$bootstrap_same_decision_rate, 0), "%", weight("bootstrap")),
    "Label" = stv_label_text(result)
  )
  data.frame(measure = names(rows), value = unname(rows))
}

#' Side-by-side summary of several results
#'
#' @param entries History entries from `stv_store_add_result()`.
#'
#' @return A data frame: `Measure`, then one column per entry, named
#'   `"#<id>"`.
#' @noRd
stv_compare_table <- function(entries) {
  out <- data.frame(Measure = c("Data", stv_result_summary(entries[[1]]$result)$measure))
  for (e in entries) {
    out[[paste0("#", e$id)]] <- c(e$data_label %||% "Unnamed data", stv_result_summary(e$result)$value)
  }
  out
}

#' Whether entries were run on the same data
#'
#' Same source and the same filtered rows.
#'
#' @param entries History entries.
#'
#' @return `TRUE` or `FALSE`.
#' @noRd
stv_same_data <- function(entries) {
  keys <- vapply(entries, \(e) paste(e$data_label %||% "", e$n_rows), character(1))
  length(unique(keys)) <= 1L
}

#' One-sentence definitions of the measures
#'
#' Shown as info popovers next to each measure. Wording follows the
#' stabilitest README.
#'
#' @return A named list of strings.
#' @noRd
stv_glossary <- function() {
  list(
    composite = paste(
      "A data-dependent score from 0 to 100: the weighted mean of the three components.",
      "It is read as fragile or robust only where stabilitest has a validated cutoff."
    ),
    jackknife = "The share of leave-one-out analyses (each observation left out in turn) that keep the original decision.",
    fragility = paste(
      "How many observations a greedy search must remove, each time the one pushing hardest toward the other decision,",
      "before the decision changes, scaled to 0 to 100."
    ),
    bootstrap = paste(
      "The share of bootstrap resamples of the data that reach the same decision.",
      "It is not the probability that a new trial will replicate the finding."
    ),
    p_value = "The p-value of the analysis on all the data. It is shown for reference and is not part of the score."
  )
}

#' The result in plain language
#'
#' Sentences for the Guided "What does this mean?" panel. Each one restates a
#' number stabilitest returned; none reads the score as fragile or robust
#' unless stabilitest returned a label.
#'
#' @param result An `stv_result`.
#'
#' @return A character vector of sentences.
#' @noRd
stv_meaning_text <- function(result) {
  m <- result@metrics
  tost <- identical(result@family, "tost")
  decision <- if (tost) {
    if (isTRUE(result@significant)) "shows the claim" else "does not show the claim"
  } else if (isTRUE(result@significant)) {
    "is statistically significant"
  } else {
    "is not statistically significant"
  }
  k <- m$worstcase_fragility_k
  removal <- if (length(k) && is.finite(k)) {
    sprintf(
      "Removing %s of the %s observations (%s%%), chosen to push the result the other way, is enough to change that decision.",
      stv_format_number(k, 0), format(result@n), stv_format_number(m$worstcase_fragility_pct)
    )
  } else {
    "Removing observations, up to the removal limit, did not change that decision."
  }
  label <- if (!is.na(result@label)) {
    sprintf(
      "Under a validated calibration, stabilitest labels this result \u201c%s\u201d.",
      result@label
    )
  } else {
    sprintf(
      paste(
        "stabilitest has no validated cutoff for this analysis, so the composite score of %s is not called",
        "fragile or robust. Read the three checks above, or compare it with other analyses of the same data."
      ),
      stv_format_number(result@score, 0)
    )
  }
  c(
    sprintf("On all the data, the analysis %s at the %s level (p = %s).", decision, format(result@alpha),
      stv_format_p(result@p_value)),
    removal,
    sprintf(
      "Leaving out one observation at a time kept the decision in %s%% of the runs.",
      stv_format_number(m$jackknife_conclusion_stability, 0)
    ),
    sprintf(
      "In %s%% of the resampled data sets the analysis reached the same decision.",
      stv_format_number(m$bootstrap_same_decision_rate, 0)
    ),
    label,
    "These checks describe this data set. They do not give the probability that a new study would repeat the result."
  )
}
