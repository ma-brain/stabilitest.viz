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
