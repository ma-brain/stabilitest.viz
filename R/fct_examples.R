#' Catalogue of bundled example data sets
#'
#' Examples come from stabilitest, plus the synthetic files in
#' `inst/extdata/synthetic/` (see `docs/synthetic-data.md`).
#'
#' @return A tibble with `id`, `group`, `label`, `description` and `file` (the
#'   synthetic file name, `NA` for stabilitest data).
#' @noRd
stv_examples <- function() {
  stabilitest_data <- tibble::tibble(
    id = c("pain_trial", "pain_ancova_trial", "onc_response_trial"),
    group = "From stabilitest",
    label = c(
      "Pain trial (two groups, change from baseline)",
      "Pain ANCOVA trial (baseline-adjusted)",
      "Oncology response trial (binary response)"
    ),
    description = c(
      paste(
        "Phase II analgesic trial: change from baseline in pain score at",
        "Week 12 (0-100 scale, negative is improvement).",
        "The two vectors pain_treatment and pain_placebo, stacked into one",
        "long table."
      ),
      paste(
        "Synthetic two-arm pain trial with baseline and Week 12 scores,",
        "suited to a baseline-adjusted linear model."
      ),
      paste(
        "Synthetic two-arm oncology trial with a 0/1 objective response,",
        "suited to a two-group proportion test."
      )
    ),
    file = NA_character_
  )

  synthetic <- tibble::tibble(
    id = c(
      "syn_two_sample", "syn_paired", "syn_binary", "syn_ancova", "syn_counts",
      "syn_survival", "syn_adsl", "syn_adeff", "syn_adtte"
    ),
    group = "Synthetic files",
    label = c(
      "Two groups, continuous outcome", "Paired measurements",
      "Two groups, binary response", "Three-arm ANCOVA with missing values",
      "Event counts with exposure", "Time to event",
      "ADaM ADSL", "ADaM BDS (pain over visits)", "ADaM ADTTE (survival)"
    ),
    description = c(
      "120 simulated subjects in two arms with a continuous outcome and two planted outliers.",
      "40 simulated subjects with before and after blood pressure.",
      "140 simulated subjects in two arms with a 0/1 responder outcome.",
      "90 simulated subjects in three arms with age, baseline and change; 4 outcomes are missing.",
      "100 simulated subjects with an event count and years of exposure.",
      "120 simulated subjects with follow-up time and an event indicator.",
      "100 simulated subjects with the usual ADSL population flags.",
      "Simulated pain scores at Baseline, Week 4 and Week 12 with BASE, CHG and ANL01FL.",
      "Simulated overall survival with AVAL and CNSR (1 = censored)."
    ),
    file = c(
      "two_sample_continuous.csv", "paired_bp.csv", "binary_response.csv",
      "ancova_trial.csv", "count_events.csv", "survival_cox.csv",
      "adsl.csv", "adeff.csv", "adtte.csv"
    )
  )

  rbind(stabilitest_data, synthetic)
}

#' Load a bundled example
#'
#' @param id One of `stv_examples()$id`.
#'
#' @return A tibble. `pain_trial` is long: one row per subject with `arm` and
#'   `change`.
#' @noRd
stv_load_example <- function(id) {
  examples <- stv_examples()
  id <- rlang::arg_match(id, examples$id)

  file <- examples$file[examples$id == id]
  if (!is.na(file)) {
    path <- app_sys("extdata", "synthetic", file)
    return(stv_read_file(path, file)$data)
  }

  switch(
    id,
    pain_trial = {
      treatment <- stabilitest::pain_treatment
      placebo <- stabilitest::pain_placebo
      tibble::tibble(
        subject_id = c(
          sprintf("PAIN-T%02d", seq_along(treatment)),
          sprintf("PAIN-P%02d", seq_along(placebo))
        ),
        arm = factor(
          rep(c("Treatment", "Placebo"), c(length(treatment), length(placebo))),
          levels = c("Treatment", "Placebo")
        ),
        change = c(treatment, placebo)
      )
    },
    pain_ancova_trial = tibble::as_tibble(stabilitest::pain_ancova_trial),
    onc_response_trial = tibble::as_tibble(stabilitest::onc_response_trial)
  )
}

#' Summarise the columns of a data frame
#'
#' @param data A data frame.
#'
#' @param labels Optional named character vector of variable labels.
#'
#' @return A tibble with `name`, `type`, `n_missing`, `pct_missing` and `label`.
#' @noRd
stv_column_summary <- function(data, labels = NULL) {
  n_missing <- vapply(data, \(x) sum(is.na(x)), integer(1), USE.NAMES = FALSE)
  label <- if (is.null(labels)) rep("", ncol(data)) else unname(labels[names(data)])
  tibble::tibble(
    name = names(data),
    type = vapply(data, stv_column_type, character(1), USE.NAMES = FALSE),
    n_missing = n_missing,
    pct_missing = if (nrow(data)) round(100 * n_missing / nrow(data), 1) else 0,
    label = ifelse(is.na(label), "", label)
  )
}

#' Short type label of a column
#'
#' @param x A vector.
#'
#' @return A single string.
#' @noRd
stv_column_type <- function(x) {
  if (is.factor(x)) {
    "factor"
  } else if (is.logical(x)) {
    "logical"
  } else if (is.integer(x)) {
    "integer"
  } else if (is.numeric(x)) {
    "numeric"
  } else if (is.character(x)) {
    "text"
  } else if (inherits(x, c("Date", "POSIXt"))) {
    "date"
  } else {
    class(x)[[1]]
  }
}

#' Read the stabilitest calibration registry
#'
#' @return A tibble, or `NULL` when the installed stabilitest does not ship
#'   the registry.
#' @noRd
stv_registry <- function() {
  path <- system.file("extdata", "calibration-registry.csv", package = "stabilitest")
  if (!nzchar(path)) {
    return(NULL)
  }
  tibble::as_tibble(utils::read.csv(path, stringsAsFactors = FALSE))
}

#' Versions of the packages the app relies on
#'
#' @return A tibble with `component` and `version`.
#' @noRd
stv_versions <- function() {
  pkgs <- c("stabilitest.viz", "stabilitest", "shiny", "bslib")
  tibble::tibble(
    Component = c(pkgs, "R"),
    Version = c(
      vapply(pkgs, \(p) as.character(getNamespaceVersion(p)), character(1)),
      paste(R.version$major, R.version$minor, sep = ".")
    )
  )
}
