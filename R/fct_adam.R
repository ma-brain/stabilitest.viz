#' First column whose name matches, ignoring case
#'
#' @param data A data frame.
#' @param candidates Column names to look for, in order of preference.
#'
#' @return The actual column name, or `NULL`.
#' @noRd
stv_pick_column <- function(data, candidates) {
  hits <- match(toupper(candidates), toupper(names(data)))
  hits <- hits[!is.na(hits)]
  if (length(hits)) names(data)[[hits[[1]]]] else NULL
}

#' Population and analysis flag columns
#'
#' Columns named like `ITTFL`, `SAFFL` or `ANL01FL` whose values are `Y`, `N`
#' or missing.
#'
#' @param data A data frame.
#'
#' @return A character vector of column names.
#' @noRd
stv_flag_columns <- function(data) {
  is_flag <- vapply(names(data), \(nm) {
    grepl("^[A-Za-z0-9]+FL$", nm) &&
      all(toupper(as.character(unique(data[[nm]]))) %in% c("Y", "N", NA_character_, ""))
  }, logical(1))
  names(data)[is_flag]
}

#' Detect CDISC ADaM structure from column names
#'
#' @param data A data frame.
#'
#' @return A list with `kind` (`"ADSL"`, `"BDS"`, `"ADTTE"` or `NA`),
#'   `label`, `evidence` and `vars`, the actual names of the key ADaM columns
#'   (`NULL` when absent).
#' @noRd
stv_detect_adam <- function(data) {
  vars <- list(
    id = stv_pick_column(data, "USUBJID"),
    trt = stv_pick_column(data, c("TRT01P", "TRTP", "TRT01A", "TRTA", "ARM", "ACTARM")),
    paramcd = stv_pick_column(data, "PARAMCD"),
    param = stv_pick_column(data, "PARAM"),
    avisit = stv_pick_column(data, "AVISIT"),
    avisitn = stv_pick_column(data, "AVISITN"),
    aval = stv_pick_column(data, "AVAL"),
    base = stv_pick_column(data, "BASE"),
    chg = stv_pick_column(data, "CHG"),
    cnsr = stv_pick_column(data, "CNSR"),
    flags = stv_flag_columns(data)
  )

  kind <- NA_character_
  evidence <- character()
  if (!is.null(vars$aval) && !is.null(vars$cnsr) && !is.null(vars$paramcd)) {
    kind <- "ADTTE"
    evidence <- c(vars$aval, vars$cnsr, vars$paramcd)
  } else if (!is.null(vars$aval) && !is.null(vars$paramcd)) {
    kind <- "BDS"
    evidence <- c(vars$aval, vars$paramcd)
  } else if (!is.null(vars$id) && !is.null(vars$trt) && is.null(vars$paramcd) && is.null(vars$aval)) {
    kind <- "ADSL"
    evidence <- c(vars$id, vars$trt)
  }

  label <- switch(
    kind %||% "none",
    ADSL = "ADaM ADSL (subject level)",
    BDS = "ADaM BDS (basic data structure)",
    ADTTE = "ADaM ADTTE (time to event)",
    "Not ADaM-structured"
  )

  list(kind = kind, label = label, evidence = evidence, vars = vars)
}

#' Default filters for a data set
#'
#' For BDS and ADTTE data the first parameter and the last visit are picked,
#' and `ANL01FL` is applied if present, so that the data are close to one row
#' per subject. The user can change all of these.
#'
#' @param data A data frame.
#' @param adam Result of `stv_detect_adam()`.
#'
#' @return A filter list, see `stv_apply_filters()`.
#' @noRd
stv_default_filters <- function(data, adam) {
  filters <- list(
    paramcd = NULL, avisit = NULL, flags = character(),
    row_col = NULL, row_vals = NULL
  )
  v <- adam$vars
  if (adam$kind %in% c("BDS", "ADTTE")) {
    filters$paramcd <- as.character(unique(data[[v$paramcd]]))[[1]]
    if (adam$kind == "BDS" && !is.null(v$avisit)) {
      filters$avisit <- utils::tail(stv_visit_levels(data, adam), 1L)
    }
    anl <- stv_pick_column(data, "ANL01FL")
    if (!is.null(anl)) filters$flags <- anl
  }
  filters
}

#' Visits in chronological order
#'
#' @param data A data frame with an `AVISIT` column.
#' @param adam Result of `stv_detect_adam()`.
#'
#' @return A character vector ordered by `AVISITN` when available.
#' @noRd
stv_visit_levels <- function(data, adam) {
  v <- adam$vars
  visits <- as.character(data[[v$avisit]])
  if (!is.null(v$avisitn)) {
    visits <- visits[order(data[[v$avisitn]])]
  }
  unique(visits[!is.na(visits)])
}

#' Apply filters to a data set
#'
#' Filter values that do not occur in the data are ignored, so a stale choice
#' never empties the data silently.
#'
#' @param data A data frame.
#' @param adam Result of `stv_detect_adam()`.
#' @param filters A list with `paramcd`, `avisit` (single values or `NULL`),
#'   `flags` (columns to keep at `"Y"`), `row_col` and `row_vals`.
#'
#' @return The filtered data frame.
#' @noRd
stv_apply_filters <- function(data, adam, filters) {
  keep <- rep(TRUE, nrow(data))
  v <- adam$vars

  match_value <- function(col, value) {
    if (is.null(col) || is.null(value) || !nzchar(value[[1]])) return(NULL)
    values <- as.character(data[[col]])
    if (!value[[1]] %in% values) return(NULL)
    values %in% value[[1]]
  }

  for (m in list(
    match_value(v$paramcd, filters$paramcd),
    match_value(v$avisit, filters$avisit)
  )) {
    if (!is.null(m)) keep <- keep & m
  }

  for (flag in intersect(filters$flags, names(data))) {
    keep <- keep & toupper(as.character(data[[flag]])) %in% "Y"
  }

  row_col <- filters$row_col
  if (length(row_col) == 1L && row_col %in% names(data) && length(filters$row_vals)) {
    keep <- keep & as.character(data[[row_col]]) %in% filters$row_vals
  }

  data[keep, , drop = FALSE]
}

#' Identify the subject column
#'
#' @param data A data frame.
#' @param adam Result of `stv_detect_adam()`.
#'
#' @return A column name, or `NULL`.
#' @noRd
stv_id_column <- function(data, adam) {
  adam$vars$id %||% {
    hits <- grep("^(subject_?id|subjid|subject|patient_?id|pid|id)$", names(data), ignore.case = TRUE)
    if (length(hits)) names(data)[[hits[[1]]]] else NULL
  }
}

#' Check that the data have one row per subject
#'
#' @param data A data frame, after filtering.
#' @param adam Result of `stv_detect_adam()`.
#'
#' @return A list with `id`, `n_rows`, `n_subjects`, `n_repeated` (subjects with
#'   more than one row) and `ok` (`NA` when no subject column was found).
#' @noRd
stv_check_subjects <- function(data, adam) {
  id <- stv_id_column(data, adam)
  if (is.null(id)) {
    return(list(id = NULL, n_rows = nrow(data), n_subjects = NA_integer_, n_repeated = NA_integer_, ok = NA))
  }
  counts <- table(data[[id]])
  list(
    id = id,
    n_rows = nrow(data),
    n_subjects = length(counts),
    n_repeated = sum(counts > 1L),
    ok = all(counts == 1L)
  )
}

#' Suggest a variable mapping
#'
#' ADaM data use the standard presets: `AVAL` or `CHG` as outcome, `TRT01P` or
#' `TRTP` as group, `BASE` as covariate, and for ADTTE `AVAL` as time with the
#' event being `1 - CNSR`. Other data use name and type heuristics. The result
#' is only a suggestion; the user confirms it in the Map step.
#'
#' @param data A data frame, after filtering.
#' @param adam Result of `stv_detect_adam()`.
#'
#' @return A tibble with `role`, `column` and `note`, zero rows when nothing
#'   could be suggested.
#' @noRd
stv_suggest_mapping <- function(data, adam) {
  rows <- if (!is.na(adam$kind) && adam$kind != "ADSL") {
    stv_adam_roles(adam)
  } else {
    stv_heuristic_roles(data, adam)
  }
  tibble::tibble(
    role = vapply(rows, `[[`, character(1), "role"),
    column = vapply(rows, `[[`, character(1), "column"),
    note = vapply(rows, \(r) r$note %||% "", character(1))
  )
}

#' @noRd
stv_role <- function(role, column, note = NULL) {
  list(role = role, column = column, note = note)
}

#' @noRd
stv_adam_roles <- function(adam) {
  v <- adam$vars
  roles <- list()
  add <- function(...) roles[[length(roles) + 1L]] <<- stv_role(...)

  if (!is.null(v$id)) add("Subject", v$id)
  if (!is.null(v$trt)) add("Group", v$trt)
  if (adam$kind == "ADTTE") {
    add("Time", v$aval)
    add("Event", v$cnsr, "Inverted: event = 1 - CNSR")
  } else {
    if (!is.null(v$chg)) {
      add("Outcome", v$chg, if (!is.null(v$aval)) paste0("Change from baseline; ", v$aval, " is the alternative"))
    } else {
      add("Outcome", v$aval)
    }
    if (!is.null(v$base)) add("Covariate", v$base, "Baseline value, for adjusted models")
  }
  roles
}

#' @noRd
stv_heuristic_roles <- function(data, adam) {
  roles <- list()
  add <- function(...) roles[[length(roles) + 1L]] <<- stv_role(...)
  used <- character()
  names_data <- names(data)
  is_cat <- vapply(data, \(x) is.character(x) || is.factor(x), logical(1))
  n_unique <- vapply(data, \(x) length(unique(x[!is.na(x)])), integer(1))
  is_binary <- vapply(data, \(x) {
    (is.numeric(x) || is.logical(x)) && all(x[!is.na(x)] %in% c(0, 1))
  }, logical(1))

  id <- stv_id_column(data, adam)
  if (!is.null(id)) {
    add("Subject", id)
    used <- c(used, id)
  }

  time <- grep("(^|_)(time|months?|days?|weeks?)(_|$)", names_data, ignore.case = TRUE, value = TRUE)
  event <- grep("^(event|status|cens|evt)", names_data, ignore.case = TRUE, value = TRUE)
  time <- setdiff(time[vapply(data[time], is.numeric, logical(1))], used)
  event <- setdiff(event[is_binary[event]], used)
  if (length(time) && length(event)) {
    add("Time", time[[1]])
    add("Event", event[[1]], "1 = event observed")
    used <- c(used, time[[1]], event[[1]])
  }

  group_pool <- setdiff(names_data[is_cat & n_unique == 2L], used)
  if (!length(group_pool)) {
    group_pool <- setdiff(names_data[is_cat & n_unique >= 3L & n_unique <= 6L], used)
  }
  if (length(group_pool)) {
    preferred <- grep("arm|group|trt|treat", group_pool, ignore.case = TRUE, value = TRUE)
    group <- c(preferred, group_pool)[[1]]
    add("Group", group, sprintf("%d levels", n_unique[[group]]))
    used <- c(used, group)
  }

  has_survival <- all(c("Time", "Event") %in% vapply(roles, `[[`, character(1), "role"))
  if (!has_survival) {
    numeric_cols <- setdiff(names_data[vapply(data, is.numeric, logical(1))], used)
    binary <- intersect(numeric_cols, names_data[is_binary])
    continuous <- setdiff(numeric_cols, names_data[is_binary])
    continuous <- continuous[n_unique[continuous] >= 3L]
    covariate_pattern <- "age|base|bmi|weight|exposure"
    candidates <- setdiff(continuous, grep(covariate_pattern, continuous, ignore.case = TRUE, value = TRUE))

    if (length(binary)) {
      add("Outcome", binary[[length(binary)]], "Binary 0/1")
      used <- c(used, binary[[length(binary)]])
    } else if (length(candidates)) {
      outcome <- candidates[[length(candidates)]]
      add("Outcome", outcome)
      used <- c(used, outcome)
    }
  }

  covariates <- setdiff(
    grep("age|base|bmi|weight", names_data, ignore.case = TRUE, value = TRUE),
    used
  )
  covariates <- covariates[vapply(data[covariates], is.numeric, logical(1))]
  for (cov in covariates) add("Covariate", cov)

  roles
}
