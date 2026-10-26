#' Catalogue of bundled example data sets
#'
#' @return A tibble with `id`, `label`, `description`.
#' @noRd
stv_examples <- function() {
  tibble::tibble(
    id = c("pain_trial", "pain_ancova_trial", "onc_response_trial"),
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
    )
  )
}

#' Load a bundled example
#'
#' @param id One of `stv_examples()$id`.
#'
#' @return A tibble. `pain_trial` is long: one row per subject with `arm` and
#'   `change`.
#' @noRd
stv_load_example <- function(id) {
  id <- rlang::arg_match(id, stv_examples()$id)

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
#' @return A tibble with `name`, `type`, `n_missing`.
#' @noRd
stv_column_summary <- function(data) {
  tibble::tibble(
    name = names(data),
    type = vapply(data, stv_column_type, character(1), USE.NAMES = FALSE),
    n_missing = vapply(data, \(x) sum(is.na(x)), integer(1), USE.NAMES = FALSE)
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
