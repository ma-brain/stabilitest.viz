#' Analysis families offered by the app
#'
#' @return A tibble with `id`, `label`, `question` (plain-language) and
#'   `stabilitest` (the function that does the work).
#' @noRd
stv_families <- function() {
  tibble::tibble(
    id = c("location", "proportion", "tost", "lm", "glm", "cox"),
    label = c(
      "Compare two groups (continuous)", "Compare two proportions",
      "Equivalence or non-inferiority", "Linear model / ANCOVA",
      "Generalised linear model", "Cox proportional hazards"
    ),
    question = c(
      "Is the difference in means or ranks fragile?",
      "Is the difference in response rates fragile?",
      "Is the claim of equivalence or non-inferiority fragile?",
      "Is the treatment effect fragile once covariates are adjusted for?",
      "Is a binary or count outcome effect fragile?",
      "Is a time-to-event effect fragile?"
    ),
    stabilitest = c(
      "robustness_analysis", "robustness_analysis", "robustness_tost",
      "robustness_lm", "robustness_glm", "robustness_surv"
    )
  )
}

#' Tests available within each family
#'
#' @param family One of `stv_families()$id`.
#'
#' @return A named character vector: label = value, or `NULL` for families
#'   without a choice.
#' @noRd
stv_tests <- function(family) {
  switch(
    family,
    location = c(
      "Welch t-test" = "t.test", "Paired t-test" = "paired.t.test",
      "Wilcoxon rank-sum" = "wilcoxon", "Wilcoxon signed-rank (paired)" = "wilcoxon.paired",
      "Brunner-Munzel" = "brunner_munzel"
    ),
    proportion = c(
      "Fisher exact" = "fisher", "Chi-square" = "chisq", "Two-sample proportions" = "prop"
    ),
    tost = c(
      "Mean difference" = "mean", "Risk difference" = "prop", "Odds ratio" = "or"
    ),
    glm = c("Binomial (logit)" = "binomial", "Poisson (log)" = "poisson"),
    NULL
  )
}

#' Default settings
#'
#' @return A list: `alpha`, `n_boot`, `max_removal_pct`,
#'   `influential_threshold`, `weights`, `seed` and `interpret`.
#' @noRd
stv_default_settings <- function() {
  list(
    alpha = 0.05,
    n_boot = 1000,
    max_removal_pct = 0.30,
    influential_threshold = 0.05,
    weights = c(jackknife = 0.4, fragility = 0.4, bootstrap = 0.2),
    seed = 123,
    interpret = TRUE
  )
}

#' Settings of the validated Fisher exact profile
#'
#' The only profile for which stabilitest returns a categorical label. The
#' values are those documented in the stabilitest README.
#'
#' @return A settings list, see `stv_default_settings()`.
#' @noRd
stv_fisher_profile_settings <- function() {
  utils::modifyList(
    stv_default_settings(),
    list(
      alpha = 0.05, n_boot = 1000, max_removal_pct = 0.30,
      weights = c(jackknife = 0, fragility = 0.5, bootstrap = 0.5)
    )
  )
}

#' Is the spec inside the validated Fisher profile?
#'
#' Only a statement about the settings. Whether stabilitest assigns a label is
#' decided by stabilitest from the data and the settings, and the app only
#' reports what it returns.
#'
#' @param spec An analysis spec.
#'
#' @return A list: `has_profile` (the method has one), `inside` (settings match
#'   it) and `message`.
#' @noRd
stv_profile_status <- function(spec) {
  if (!identical(spec$family, "proportion") || !identical(spec$test, "fisher")) {
    return(list(
      has_profile = FALSE, inside = NA,
      message = "No validated calibration profile exists for this method. The result is numeric only."
    ))
  }
  s <- spec$settings %||% stv_default_settings()
  ref <- stv_fisher_profile_settings()
  inside <- isTRUE(all.equal(s$alpha, ref$alpha)) &&
    isTRUE(all.equal(as.numeric(s$n_boot), ref$n_boot)) &&
    isTRUE(all.equal(s$max_removal_pct, ref$max_removal_pct)) &&
    isTRUE(all.equal(unname(s$weights[names(ref$weights)]), unname(ref$weights)))
  list(
    has_profile = TRUE, inside = inside,
    message = if (inside) {
      "Settings match the validated Fisher profile. stabilitest decides whether the data are eligible for a label."
    } else {
      "Settings are outside the validated Fisher profile, so the result will be numeric only."
    }
  )
}

#' Rough runtime estimate
#'
#' Seconds per (observation + bootstrap resample), measured on a laptop. It is
#' only meant to warn about long runs, not to predict them.
#'
#' @param spec An analysis spec.
#' @param n Number of rows analysed.
#'
#' @return Estimated seconds.
#' @noRd
stv_estimate_runtime <- function(spec, n) {
  rate <- switch(
    spec$family,
    location = if (spec$test %in% c("wilcoxon", "wilcoxon.paired")) 2e-2 else 6e-4,
    proportion = 1.2e-3,
    tost = 5e-4,
    lm = 1.2e-3,
    glm = 2.1e-3,
    cox = 2.7e-3
  )
  rate * (n + spec$settings$n_boot) * max(n / 100, 1)
}

#' Columns by the roles they can take
#'
#' Used both to fill the Map choices and to check a spec, so the two agree.
#' Only the type and structure of a column are looked at, never its values'
#' statistics.
#'
#' @param data A data frame.
#' @param ids Columns known to identify subjects (from the suggested mapping).
#'
#' @return A list of column-name vectors:
#'   * `all`: every column.
#'   * `id`: identifiers: `ids`, plus text columns with a different value on
#'     every row.
#'   * `constant`: a single value (or none).
#'   * `numeric`: numeric, not an identifier.
#'   * `binary`: 0/1 or logical.
#'   * `measure`: numeric with more than two values, not an identifier: a
#'     continuous outcome or measurement.
#'   * `response`: can be read as response / no response: 0/1, two numeric
#'     values, or a text column with 2 to 20 values.
#'   * `group`: can split rows into groups: text, 0/1, or up to 8 numeric
#'     values; not an identifier.
#'   * `count`: non-negative whole numbers.
#'   * `positive`: numeric, every value above 0.
#'   * `nonnegative`: numeric, every value at least 0.
#'   * `predictor`: neither an identifier nor constant.
#' @noRd
stv_column_kinds <- function(data, ids = character()) {
  present <- lapply(data, \(x) x[!is.na(x)])
  n_unique <- vapply(present, \(x) length(unique(x)), integer(1))
  n_present <- lengths(present)
  is_num <- vapply(data, is.numeric, logical(1))
  is_text <- vapply(data, \(x) is.character(x) || is.factor(x), logical(1))
  is_binary <- vapply(present, \(x) {
    (is.numeric(x) || is.logical(x)) && length(x) > 0L && all(x %in% c(0, 1))
  }, logical(1))
  all_values <- \(test) vapply(present, \(x) is.numeric(x) && length(x) > 0L && all(test(x)), logical(1))
  is_id <- names(data) %in% ids | (is_text & n_unique == n_present & n_present > 2L)
  is_const <- n_unique <= 1L
  cols <- \(keep) names(data)[keep]

  list(
    all = names(data),
    id = cols(is_id),
    constant = cols(is_const),
    numeric = cols(is_num & !is_id),
    binary = cols(is_binary),
    measure = cols(is_num & !is_binary & n_unique > 2L & !is_id),
    response = cols(!is_id & (is_binary | (is_num & n_unique == 2L) | (is_text & n_unique >= 2L & n_unique <= 20L))),
    group = cols(!is_id & (is_text & n_unique >= 2L | is_binary | (n_unique >= 2L & n_unique <= 8L))),
    count = cols(!is_id & all_values(\(x) x >= 0 & x == round(x))),
    positive = cols(!is_id & all_values(\(x) x > 0)),
    nonnegative = cols(!is_id & all_values(\(x) x >= 0)),
    predictor = cols(!is_id & !is_const)
  )
}

#' Check a spec against the data
#'
#' @param spec An analysis spec, see `stv_spec_new()`.
#' @param data The filtered data frame.
#' @param max_n_boot Limit from the configuration profile.
#' @param checks Result of `stv_check_subjects()`, or `NULL`.
#'
#' @return A character vector of problems, empty when the spec can run.
#' @noRd
stv_validate_spec <- function(spec, data, max_n_boot = Inf, checks = NULL) {
  problems <- character()
  add <- function(...) problems <<- c(problems, paste0(...))

  if (is.null(spec$family) || !spec$family %in% stv_families()$id) {
    return("Choose what to test first.")
  }
  if (is.null(data) || !nrow(data)) {
    return("Load data first.")
  }

  s <- spec$settings
  if (!is.numeric(s$alpha) || length(s$alpha) != 1L || !is.finite(s$alpha) || s$alpha <= 0 || s$alpha >= 1) {
    add("The significance level must be between 0 and 1.")
  }
  if (!is.numeric(s$n_boot) || length(s$n_boot) != 1L || !is.finite(s$n_boot) || s$n_boot < 1 || s$n_boot != round(s$n_boot)) {
    add("The number of bootstrap resamples must be a whole number of at least 1.")
  } else if (s$n_boot > max_n_boot) {
    add("The number of bootstrap resamples is limited to ", max_n_boot, " here.")
  }
  if (!is.numeric(s$max_removal_pct) || length(s$max_removal_pct) != 1L || !is.finite(s$max_removal_pct) ||
    s$max_removal_pct <= 0 || s$max_removal_pct > 0.5) {
    add("The maximum removal must be above 0% and at most 50%.")
  }
  w <- s$weights
  if (is.null(w) || !is.numeric(w) || anyNA(w) || any(w < 0) ||
    !setequal(names(w), c("jackknife", "fragility", "bootstrap")) || abs(sum(w) - 1) > 1e-6) {
    add("The three score weights must be non-negative and add up to 1.")
  }
  if (!is.numeric(s$seed) || length(s$seed) != 1L || !is.finite(s$seed)) {
    add("The seed must be a number.")
  }

  kinds <- stv_column_kinds(data)
  is_set <- \(col) !is.null(col) && length(col) == 1L && !is.na(col) && nzchar(col)
  has_col <- function(col, what) {
    if (!is_set(col)) {
      add("Choose the ", what, ".")
      return(FALSE)
    }
    if (!col %in% names(data)) {
      add("The ", what, " column '", col, "' is not in the data.")
      return(FALSE)
    }
    TRUE
  }
  not_id <- function(col, what) {
    if (col %in% kinds$id) {
      add("'", col, "' identifies subjects (a different value on every row), so it cannot be the ", what, ".")
      return(FALSE)
    }
    TRUE
  }
  # Each column may play one role only
  distinct_roles <- function(roles) {
    roles <- roles[vapply(roles, is_set, logical(1))]
    dup <- unique(unlist(roles)[duplicated(unlist(roles))])
    for (col in dup) {
      add("'", col, "' is used as both the ", paste(names(roles)[unlist(roles) == col], collapse = " and the "), ".")
    }
  }
  needs_measure <- function(col, what, binary_hint) {
    if (!has_col(col, what) || !not_id(col, what)) return()
    if (!is.numeric(data[[col]])) {
      add("The ", what, " '", col, "' must be numeric.")
    } else if (col %in% kinds$binary) {
      add("The ", what, " '", col, "' only takes the values 0 and 1. ", binary_hint)
    } else if (!col %in% kinds$measure) {
      add("The ", what, " '", col, "' has fewer than three different values, so it is not a measurement.")
    }
  }
  needs_response <- function(col, what, success = NULL) {
    if (!has_col(col, what) || !not_id(col, what)) return()
    x <- data[[col]]
    x <- x[!is.na(x)]
    if (!col %in% kinds$response) {
      add(
        "The ", what, " '", col, "' is not a response: it needs 0/1 values or a few categories. ",
        "For a measurement, use Compare two groups (continuous)."
      )
    } else if (is_set(success)) {
      if (!success %in% as.character(x)) add("The success value '", success, "' does not occur in '", col, "'.")
    } else if (!col %in% kinds$binary) {
      add("Pick which value of '", col, "' counts as a response.")
    }
  }

  group_cols <- function(outcome) {
    if (!has_col(spec$group, "group column") || !not_id(spec$group, "group column")) return(invisible())
    if (!spec$group %in% kinds$group) {
      add("'", spec$group, "' has too many different values to define groups.")
      return(invisible())
    }
    lv <- spec$levels
    present <- unique(as.character(stats::na.omit(data[[spec$group]])))
    if (length(lv) != 2L || anyDuplicated(lv) || anyNA(lv)) {
      add("Pick two different groups to compare.")
    } else if (!all(lv %in% present)) {
      add("The groups to compare are not both present in '", spec$group, "'.")
    } else if (is_set(outcome) && outcome %in% names(data)) {
      for (level in lv) {
        rows <- as.character(data[[spec$group]]) %in% level
        if (!any(!is.na(data[[outcome]][rows]))) add("Group '", level, "' has no rows with a value for '", outcome, "'.")
      }
    }
  }

  if (spec$family %in% c("location", "proportion", "tost")) {
    endpoint_binary <- spec$family == "proportion" || (spec$family == "tost" && spec$test %in% c("prop", "or"))
    paired <- spec$family == "location" && spec$test %in% c("paired.t.test", "wilcoxon.paired") ||
      spec$family == "tost" && isTRUE(spec$paired)
    layout <- spec$paired_layout %||% "wide"
    binary_hint <- if (spec$family == "tost") {
      "Use the risk difference or odds ratio endpoint."
    } else {
      "Use Compare two proportions."
    }

    if (paired && layout == "wide") {
      needs_measure(spec$outcome, "first measurement", "Paired tests here need measurements, not 0/1 values.")
      needs_measure(spec$outcome2, "second measurement", "Paired tests here need measurements, not 0/1 values.")
      if (is_set(spec$outcome) && identical(spec$outcome, spec$outcome2)) {
        add("The two measurements must be different columns.")
      }
    } else {
      if (endpoint_binary) {
        needs_response(spec$outcome, "outcome", spec$success)
      } else {
        needs_measure(spec$outcome, "outcome", binary_hint)
      }
      distinct_roles(list(outcome = spec$outcome, `group column` = spec$group, `pair identifier` = if (paired) spec$pair_id))
      if (!identical(spec$outcome, spec$group)) group_cols(spec$outcome)
      if (paired && has_col(spec$pair_id, "pair identifier")) {
        lv <- spec$levels
        ok_cols <- is_set(spec$group) && spec$group %in% names(data)
        if (ok_cols && length(lv) == 2L) {
          rows <- data[[spec$group]] %in% lv
          ids <- split(data[[spec$pair_id]][rows], data[[spec$group]][rows])
          if (any(vapply(ids, anyDuplicated, integer(1)) > 0L) || !setequal(ids[[lv[[1]]]], ids[[lv[[2]]]])) {
            add("Each pair identifier must appear exactly once in each group.")
          }
        }
      }
    }

    if (spec$family == "tost") {
      is_equiv <- identical(spec$tost_type, "equivalence")
      asym <- is_equiv && !is.null(spec$delta_L) && !is.null(spec$delta_U)
      if (!spec$tost_type %in% c("equivalence", "noninferiority")) add("Choose equivalence or non-inferiority.")
      if (asym) {
        if (!is.numeric(spec$delta_L) || !is.numeric(spec$delta_U) || !(spec$delta_L < spec$delta_U)) {
          add("The lower margin must be below the upper margin.")
        }
      } else if (!is.numeric(spec$margin) || length(spec$margin) != 1L || !is.finite(spec$margin) || spec$margin <= 0) {
        add("Enter a margin above 0.")
      }
      if (identical(spec$test, "or") && is.numeric(spec$margin) && length(spec$margin) == 1L && !asym && spec$margin <= 1) {
        add("An odds-ratio margin must be above 1.")
      }
      if (identical(spec$test, "prop") && is.numeric(spec$margin) && length(spec$margin) == 1L && !asym && spec$margin >= 1) {
        add("A risk-difference margin is a difference in proportions, so it must be below 1.")
      }
    }
  } else {
    if (spec$family == "cox") {
      if (has_col(spec$time, "time column") && not_id(spec$time, "time column")) {
        if (!spec$time %in% kinds$positive) add("The time column '", spec$time, "' must be numeric with every value above 0.")
      }
      if (has_col(spec$event, "event column") && not_id(spec$event, "event column")) {
        ev <- data[[spec$event]]
        if (!spec$event %in% kinds$binary) {
          add("The event column '", spec$event, "' must be 0/1.")
        } else {
          events <- if (isTRUE(spec$event_invert)) 1 - ev else ev
          if (!any(events == 1, na.rm = TRUE)) add("The event column '", spec$event, "' records no events.")
        }
      }
    } else if (has_col(spec$outcome, "outcome") && not_id(spec$outcome, "outcome")) {
      if (spec$family == "lm") {
        needs_measure(spec$outcome, "outcome", "Use Generalised linear model with Binomial (logit).")
      } else if (identical(spec$test, "binomial") && !spec$outcome %in% kinds$binary) {
        add("A binomial outcome must be 0/1. '", spec$outcome, "' is not.")
      } else if (identical(spec$test, "poisson") && !spec$outcome %in% kinds$count) {
        add("A count outcome must be non-negative whole numbers. '", spec$outcome, "' is not.")
      }
    }

    preds <- spec$predictors
    if (!length(preds)) {
      add("Choose at least one predictor.")
    } else if (!all(preds %in% names(data))) {
      add("A predictor is not in the data.")
    } else {
      for (col in intersect(preds, kinds$id)) {
        add("'", col, "' identifies subjects, so it cannot be a predictor.")
      }
      for (col in intersect(preds, kinds$constant)) {
        add("The predictor '", col, "' has a single value, so it has no effect to estimate.")
      }
    }
    offset <- if (is_set(spec$offset)) spec$offset
    weights <- if (is_set(spec$obs_weights)) spec$obs_weights
    if (!is.null(offset) && spec$family != "glm") add("An offset is only used for generalised linear models.")
    if (!is.null(offset) && has_col(offset, "exposure column") && !offset %in% kinds$positive) {
      add("The exposure column '", offset, "' must be above 0 on every row (its log is the offset).")
    }
    if (!is.null(weights) && has_col(weights, "weights column") && !weights %in% kinds$nonnegative) {
      add("Observation weights in '", weights, "' must be numeric and at least 0.")
    }
    roles <- list(outcome = spec$outcome, `time column` = spec$time, `event column` = spec$event,
      `exposure column` = offset, `weights column` = weights)
    distinct_roles(roles)
    for (col in intersect(preds, unlist(roles[vapply(roles, is_set, logical(1))]))) {
      add("'", col, "' cannot be a predictor and also the ", names(roles)[vapply(roles, \(r) identical(r, col), logical(1))][[1]], ".")
    }

    if (!length(spec$term) || is.na(spec$term) || !nzchar(spec$term)) {
      add("Choose the model term to test.")
    } else if (length(preds) && all(preds %in% names(data)) &&
      !spec$term %in% stv_term_choices(spec, data)$all) {
      add("The term '", spec$term, "' is not in the model.")
    }
  }

  if (!length(problems) && nrow(data) < 10L) {
    add("At least 10 rows are needed.")
  }
  if (!length(problems) && !is.null(checks) && isFALSE(checks$ok) && !identical(spec$paired_layout, "long")) {
    add("The data have more than one row per subject. Pick a parameter and visit, or filter rows, in the Data step.")
  }

  problems
}

#' Columns a spec uses
#'
#' @param spec An analysis spec.
#'
#' @return A character vector of column names.
#' @noRd
stv_spec_columns <- function(spec) {
  cols <- switch(
    spec$family,
    location = , proportion = , tost = {
      paired_wide <- (spec$family == "location" && spec$test %in% c("paired.t.test", "wilcoxon.paired") ||
        spec$family == "tost" && isTRUE(spec$paired)) && identical(spec$paired_layout %||% "wide", "wide")
      if (paired_wide) c(spec$outcome, spec$outcome2) else c(spec$outcome, spec$group, spec$pair_id)
    },
    lm = , glm = c(spec$outcome, spec$predictors, spec$offset, spec$obs_weights),
    cox = c(spec$time, spec$event, spec$predictors, spec$obs_weights)
  )
  unique(cols[!is.na(cols) & nzchar(cols)])
}

#' Coefficient and term names of the model in a spec
#'
#' Enumerates the names `stabilitest` accepts for `term` by building the design
#' matrix of the right-hand side. No model is fitted.
#'
#' @param spec A model spec (`lm`, `glm` or `cox`).
#' @param data The filtered data frame.
#'
#' @return A list: `terms` (term labels), `coefficients` and `all`.
#' @noRd
stv_term_choices <- function(spec, data) {
  empty <- list(terms = character(), coefficients = character(), all = character())
  preds <- spec$predictors
  if (!length(preds) || !all(preds %in% names(data))) {
    return(empty)
  }
  prepared <- tryCatch(
    stv_apply_prep(stv_prep_exprs(spec), data)$data[, preds, drop = FALSE],
    error = \(e) NULL
  )
  if (is.null(prepared) || !nrow(prepared)) {
    return(empty)
  }
  rhs <- stats::reformulate(vapply(preds, \(p) deparse(as.name(p), backtick = TRUE), character(1)))
  coefs <- tryCatch(
    setdiff(colnames(stats::model.matrix(rhs, data = prepared)), "(Intercept)"),
    error = \(e) character()
  )
  labels <- attr(stats::terms(rhs), "term.labels")
  list(terms = labels, coefficients = coefs, all = unique(c(labels, coefs)))
}

#' Suggest an analysis family from the suggested mapping
#'
#' @param suggestion Result of `stv_suggest_mapping()`.
#' @param data The filtered data frame.
#'
#' @return One of `stv_families()$id`.
#' @noRd
stv_suggest_family <- function(suggestion, data) {
  roles <- if (is.null(suggestion)) character() else suggestion$role
  if (all(c("Time", "Event") %in% roles)) {
    return("cox")
  }
  outcome <- suggestion$column[suggestion$role == "Outcome"]
  if (length(outcome) && outcome[[1]] %in% names(data)) {
    y <- data[[outcome[[1]]]]
    if ((is.numeric(y) || is.logical(y)) && all(y[!is.na(y)] %in% c(0, 1))) {
      return("proportion")
    }
  }
  "location"
}

#' Suggest a test within the suggested family
#'
#' Data with no group column and at least two numeric measurement columns
#' (for example before and after) is read as paired.
#'
#' @inheritParams stv_suggest_family
#' @param family The suggested family.
#'
#' @return A test id from `stv_tests(family)`, or `NULL` for the default.
#' @noRd
stv_suggest_test <- function(suggestion, data, family) {
  roles <- if (is.null(suggestion)) character() else suggestion$role
  if (!identical(family, "location") || "Group" %in% roles) {
    return(NULL)
  }
  ids <- suggestion$column[roles == "Subject"]
  measurements <- setdiff(names(data)[vapply(data, is.numeric, logical(1))], ids)
  if (length(measurements) >= 2L) "paired.t.test" else NULL
}

#' Create an analysis spec
#'
#' @param family One of `stv_families()$id`.
#' @param test Test or endpoint within the family, see `stv_tests()`.
#' @param ... Further spec fields, for example `outcome`, `group`, `levels`.
#' @param settings Named list overriding `stv_default_settings()`.
#'
#' @return A list.
#' @noRd
stv_spec_new <- function(family, test = NULL, ..., settings = list()) {
  family <- rlang::arg_match(family, stv_families()$id)
  s <- utils::modifyList(stv_default_settings(), settings)
  c(list(family = family, test = test), list(...), list(settings = s))
}

#' Complete a spec with facts about the data
#'
#' Records which predictors are text or logical, so that the plan converts them
#' to factors.
#'
#' @param spec An analysis spec.
#' @param data The filtered data frame.
#'
#' @return The spec with `factors` set.
#' @noRd
stv_spec_finalize <- function(spec, data) {
  preds <- intersect(spec$predictors, names(data))
  spec$factors <- preds[vapply(data[preds], \(x) is.character(x) || is.logical(x), logical(1))]
  spec
}
