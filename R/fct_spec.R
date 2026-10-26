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

  has_col <- function(col, what) {
    if (is.null(col) || !length(col) || is.na(col) || !nzchar(col)) {
      add("Choose the ", what, ".")
      return(FALSE)
    }
    if (!col %in% names(data)) {
      add("The ", what, " column '", col, "' is not in the data.")
      return(FALSE)
    }
    TRUE
  }
  needs_numeric <- function(col, what) {
    if (has_col(col, what) && !is.numeric(data[[col]])) {
      add("The ", what, " '", col, "' must be numeric.")
    }
  }
  needs_binary <- function(col, what, success = NULL) {
    if (!has_col(col, what)) return()
    x <- data[[col]]
    x <- x[!is.na(x)]
    if (!is.null(success) && nzchar(success)) {
      if (!success %in% as.character(x)) add("The success value '", success, "' does not occur in '", col, "'.")
    } else if (!(is.numeric(x) || is.logical(x)) || !all(x %in% c(0, 1))) {
      add("The ", what, " '", col, "' must be 0/1 or logical, or pick which value counts as success.")
    }
  }

  group_cols <- function() {
    if (!has_col(spec$group, "group column")) return(invisible())
    lv <- spec$levels
    present <- unique(as.character(stats::na.omit(data[[spec$group]])))
    if (length(lv) != 2L || anyDuplicated(lv) || anyNA(lv)) {
      add("Pick two different groups to compare.")
    } else if (!all(lv %in% present)) {
      add("The groups to compare are not both present in '", spec$group, "'.")
    }
  }

  if (spec$family %in% c("location", "proportion", "tost")) {
    endpoint_binary <- spec$family == "proportion" || (spec$family == "tost" && spec$test %in% c("prop", "or"))
    paired <- spec$family == "location" && spec$test %in% c("paired.t.test", "wilcoxon.paired") ||
      spec$family == "tost" && isTRUE(spec$paired)
    layout <- spec$paired_layout %||% "wide"

    if (paired && layout == "wide") {
      needs_numeric(spec$outcome, "first measurement")
      needs_numeric(spec$outcome2, "second measurement")
      if (!is.null(spec$outcome) && identical(spec$outcome, spec$outcome2)) {
        add("The two measurements must be different columns.")
      }
    } else {
      if (endpoint_binary) needs_binary(spec$outcome, "outcome", spec$success) else needs_numeric(spec$outcome, "outcome")
      group_cols()
      if (paired && has_col(spec$pair_id, "pair identifier")) {
        lv <- spec$levels
        ok_cols <- !is.null(spec$group) && spec$group %in% names(data)
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
    }
  } else {
    if (has_col(spec$outcome, "outcome")) {
      y <- data[[spec$outcome]]
      if (spec$family == "lm" && !is.numeric(y)) add("The outcome '", spec$outcome, "' must be numeric.")
      if (spec$family == "glm" && identical(spec$test, "binomial")) needs_binary(spec$outcome, "outcome")
      if (spec$family == "glm" && identical(spec$test, "poisson") &&
        (!is.numeric(y) || any(y < 0, na.rm = TRUE) || any(y != round(y), na.rm = TRUE))) {
        add("A count outcome must be non-negative whole numbers.")
      }
    }
    if (spec$family == "cox") {
      needs_numeric(spec$time, "time column")
      if (has_col(spec$event, "event column")) {
        ev <- data[[spec$event]]
        if (!(is.numeric(ev) || is.logical(ev)) || !all(ev[!is.na(ev)] %in% c(0, 1))) {
          add("The event column '", spec$event, "' must be 0/1.")
        }
      }
    }
    preds <- spec$predictors
    if (!length(preds)) {
      add("Choose at least one predictor.")
    } else if (!all(preds %in% names(data))) {
      add("A predictor is not in the data.")
    }
    for (col in c(spec$offset, spec$obs_weights)) {
      if (!is.null(col) && nzchar(col) && !col %in% names(data)) add("Column '", col, "' is not in the data.")
    }
    if (!is.null(spec$offset) && nzchar(spec$offset) && !spec$family == "glm") add("An offset is only used for generalised linear models.")
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
