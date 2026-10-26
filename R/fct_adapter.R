#' The app's internal result
#'
#' stabilitest returns two layouts: `robustness_analysis` is nested and
#' `robustness_model` (also used by TOST) is flat. This class normalises both so
#' that every results module is written once.
#'
#' Tables use the same columns for every family. `row` is the row number in the
#' filtered data shown in the Data step.
#'
#' @details
#' `label` is only ever what stabilitest returned in `interpretation_label`, and
#' only when its calibration says the result is eligible. It is `NA` otherwise,
#' in which case `calibration_status` and `calibration_message` explain why.
#' The app never derives a label from `score`.
#'
#' @noRd
stv_result <- S7::new_class(
  "stv_result",
  properties = list(
    family = S7::class_character,
    method = S7::class_character,
    method_label = S7::class_character,
    spec = S7::class_list,
    n = S7::class_numeric,
    estimate = S7::class_numeric,
    ci = S7::class_numeric,
    p_value = S7::class_numeric,
    alpha = S7::class_numeric,
    significant = S7::class_logical,
    score = S7::class_numeric,
    metrics = S7::class_data.frame,
    jackknife = S7::class_data.frame,
    removal = S7::class_data.frame,
    extreme = S7::new_union(NULL, S7::class_data.frame),
    bootstrap = S7::class_data.frame,
    observations = S7::class_data.frame,
    removed_rows = S7::class_integer,
    label = S7::class_character,
    calibration = S7::class_list,
    interpretation = S7::new_union(NULL, S7::class_list),
    code = S7::class_character,
    elapsed = S7::class_numeric,
    created = S7::class_any,
    raw = S7::class_any
  ),
  validator = function(self) {
    if (length(self@label) != 1L) {
      return("`label` must have length 1 (NA when there is no validated label).")
    }
    if (!is.na(self@label) && !isTRUE(self@calibration$applicable)) {
      return("A label requires an applicable calibration.")
    }
    NULL
  }
)

#' Calibration status of a result
#'
#' @param result An `stv_result`.
#'
#' @return One of `"validated_label"` (stabilitest returned a label) or
#'   `"numeric_only"`.
#' @noRd
stv_calibration_state <- function(result) {
  if (is.na(result@label)) "numeric_only" else "validated_label"
}

#' Message for the calibration banner
#'
#' Neutral text when there is no label, never an inferred reading of the score.
#'
#' @param result An `stv_result`.
#'
#' @return A string.
#' @noRd
stv_calibration_message <- function(result) {
  if (!is.na(result@label)) {
    return(paste0("stabilitest label: ", result@label, "."))
  }
  "Numeric only, no validated cutoff for this result. The score and its components are shown without a categorical label, and a lower or higher score is not read as fragile or robust."
}

#' Normalise a resampling table
#'
#' @param x A tibble from stabilitest.
#' @param columns Output columns, filled with `NA` when missing.
#'
#' @return A tibble with exactly `columns`.
#' @noRd
stv_normalise_table <- function(x, columns) {
  x <- as.data.frame(x)
  for (col in setdiff(columns, names(x))) {
    x[[col]] <- NA
  }
  tibble::as_tibble(x[, columns, drop = FALSE])
}

#' Map jackknife units to rows of the filtered data
#'
#' @param spec An analysis spec.
#' @param prepared Prepared data.
#' @param row_map Row numbers of the prepared rows in the filtered data.
#' @param n_units Number of jackknife units.
#'
#' @return A tibble with `unit`, `label` and `row`.
#' @noRd
stv_observation_map <- function(spec, prepared, row_map, n_units) {
  if (spec$family %in% c("lm", "glm", "cox")) {
    return(tibble::tibble(
      unit = seq_len(n_units),
      label = paste0("row-", row_map[seq_len(n_units)]),
      row = row_map[seq_len(n_units)]
    ))
  }

  if (stv_is_paired(spec)) {
    rows <- if (identical(spec$paired_layout %||% "wide", "wide")) {
      row_map
    } else {
      row_map[prepared[[spec$group]] == spec$levels[[1]]]
    }
    return(tibble::tibble(
      unit = seq_along(rows), label = paste0("pair-", seq_along(rows)), row = rows
    ))
  }

  rows1 <- row_map[prepared[[spec$group]] == spec$levels[[1]]]
  rows2 <- row_map[prepared[[spec$group]] == spec$levels[[2]]]
  tibble::tibble(
    unit = seq_len(length(rows1) + length(rows2)),
    label = c(paste0("G1-", seq_along(rows1)), paste0("G2-", seq_along(rows2))),
    row = c(rows1, rows2)
  )
}

#' Human-readable method name
#'
#' @param spec An analysis spec.
#' @param raw The stabilitest result.
#'
#' @return A string.
#' @noRd
stv_method_label <- function(spec, raw) {
  tests <- stv_tests(spec$family)
  family_label <- stv_families()$label[stv_families()$id == spec$family]
  if (is.null(tests)) {
    return(family_label)
  }
  paste0(family_label, ": ", names(tests)[tests == spec$test])
}

#' Convert a stabilitest result to an `stv_result`
#'
#' @param raw Result of a stabilitest call.
#' @param spec The analysis spec that produced it.
#' @param plan The plan that produced it.
#' @param prepared Prepared data.
#' @param row_map Row numbers of the prepared rows in the filtered data.
#' @param elapsed Seconds the run took.
#'
#' @return An `stv_result`.
#' @noRd
stv_as_result <- function(raw, spec, plan, prepared, row_map, elapsed = NA_real_) {
  is_nested <- !inherits(raw, "robustness_model")

  metrics <- tibble::as_tibble(raw$metrics)
  names(metrics)[names(metrics) == "bootstrap_reproducibility"] <- "bootstrap_same_decision_rate"

  jackknife_raw <- if (is_nested) raw$jackknife$results else raw$jackknife
  worstcase_raw <- if (is_nested) raw$worstcase$results else raw$worstcase
  bootstrap_raw <- if (is_nested) raw$bootstrap$results else raw$bootstrap

  observations <- stv_observation_map(spec, prepared, row_map, nrow(jackknife_raw))
  unit_rows <- function(units) observations$row[match(units, observations$unit)]

  unit_col <- if (is_nested) jackknife_raw$unit else jackknife_raw$row
  jackknife <- stv_normalise_table(
    transform(
      as.data.frame(jackknife_raw),
      unit = unit_col,
      label = observations$label[match(unit_col, observations$unit)],
      row = unit_rows(unit_col)
    ),
    c("unit", "label", "row", "p_value", "statistic", "estimate", "significant",
      "conclusion_match", "influential_delta", "influential")
  )
  path_columns <- c("k_removed", "p_value", "statistic", "estimate", "significant", "conclusion_match")
  boot_columns <- c("iteration", "p_value", "statistic", "estimate", "significant", "conclusion_match")

  removed_rows <- if (is_nested) integer() else unit_rows(as.integer(raw$removed_rows))

  label <- raw$interpretation_label
  label <- if (is.null(label) || length(label) != 1L || is.na(label) || !nzchar(label) ||
    !isTRUE(raw$calibration$applicable)) {
    NA_character_
  } else {
    as.character(label)
  }

  estimate <- raw$original_estimate %||% raw$original_mean_diff %||% NA_real_
  ci <- raw$original_ci %||% c(NA_real_, NA_real_)

  stv_result(
    family = spec$family,
    method = raw$calibration$calibration_unit %||% NA_character_,
    method_label = stv_method_label(spec, raw),
    spec = spec,
    n = raw$n,
    estimate = as.numeric(estimate),
    ci = as.numeric(ci),
    p_value = raw$original_p,
    alpha = raw$alpha,
    significant = raw$original_significant,
    score = metrics$overall_robustness,
    metrics = metrics,
    jackknife = jackknife,
    removal = stv_normalise_table(worstcase_raw, path_columns),
    extreme = if (is_nested) stv_normalise_table(raw$extreme$results, path_columns) else NULL,
    bootstrap = stv_normalise_table(bootstrap_raw, boot_columns),
    observations = observations,
    removed_rows = removed_rows,
    label = label,
    calibration = as.list(raw$calibration),
    interpretation = if (is_nested && is.list(raw$interpretation)) raw$interpretation else NULL,
    code = stv_codegen(spec),
    elapsed = as.numeric(elapsed),
    created = Sys.time(),
    raw = raw
  )
}
