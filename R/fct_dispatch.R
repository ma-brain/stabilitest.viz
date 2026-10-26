# All calls to stabilitest go through this file. A spec is turned into a plan
# made of R language objects. The same plan is evaluated to run the analysis
# and printed to give the reproducible code, so the two cannot drift apart.

#' Column of the data as a call
#'
#' @param col Column name.
#'
#' @return The call `data[["col"]]`.
#' @noRd
stv_col <- function(col) {
  call("[[", quote(data), col)
}

#' Rows of an outcome column that belong to one group
#'
#' @param outcome,group Column names.
#' @param level The group level.
#' @param success `NULL`, or the value of `outcome` that counts as success.
#'
#' @return A call.
#' @noRd
stv_group_values <- function(outcome, group, level, success = NULL) {
  values <- stv_col(outcome)
  if (!is.null(success) && nzchar(success)) {
    values <- call("as.numeric", call("==", values, success))
  }
  call("[", values, call("==", stv_col(group), level))
}

#' Call a stabilitest function
#'
#' @param fun Name of the exported function.
#' @param args Named list of arguments (values or calls).
#'
#' @return A call `stabilitest::fun(...)`.
#' @noRd
stv_stabilitest_call <- function(fun, args) {
  as.call(c(list(call("::", as.name("stabilitest"), as.name(fun))), args))
}

#' Is the spec a paired design?
#'
#' @param spec An analysis spec.
#'
#' @return A single logical.
#' @noRd
stv_is_paired <- function(spec) {
  (spec$family == "location" && spec$test %in% c("paired.t.test", "wilcoxon.paired")) ||
    (spec$family == "tost" && isTRUE(spec$paired))
}

#' Data preparation steps of a spec
#'
#' Complete cases on the columns used, optional reference levels, and for
#' paired data in long layout ordering by pair identifier. Row names keep the
#' row numbers of the filtered data, which is how results link back to them.
#'
#' @param spec An analysis spec.
#'
#' @return A list of expressions that update `data`.
#' @noRd
stv_prep_exprs <- function(spec) {
  cols <- stv_spec_columns(spec)
  steps <- list(
    quote(data <- as.data.frame(data)),
    bquote(data <- data[stats::complete.cases(data[, .(cols)]), , drop = FALSE])
  )

  if (spec$family %in% c("lm", "glm", "cox")) {
    # stabilitest's model functions need factors rather than text columns
    reference <- spec$reference %||% list()
    for (col in union(spec$factors %||% character(), names(reference))) {
      level <- reference[[col]]
      steps <- c(steps, list(
        if (!is.null(level) && nzchar(level)) {
          bquote(data[[.(col)]] <- stats::relevel(factor(data[[.(col)]]), ref = .(level)))
        } else {
          bquote(data[[.(col)]] <- factor(data[[.(col)]]))
        }
      ))
    }
  } else if (stv_is_paired(spec) && identical(spec$paired_layout, "long")) {
    steps <- c(steps, list(bquote(data <- data[order(data[[.(spec$pair_id)]]), , drop = FALSE])))
  }
  steps
}

#' Run the preparation steps
#'
#' @param steps Expressions from `stv_prep_exprs()`.
#' @param data The filtered data frame.
#'
#' @return A list with `data` (prepared data frame) and `row_map` (the row
#'   number in `data` of each prepared row).
#' @noRd
stv_apply_prep <- function(steps, data) {
  env <- new.env(parent = baseenv())
  env$data <- data
  for (step in steps) {
    eval(step, env)
  }
  list(data = env$data, row_map = as.integer(rownames(env$data)))
}

#' Model formula of a spec
#'
#' @param spec A model spec (`lm`, `glm` or `cox`).
#'
#' @return A call that evaluates to a formula, for example
#'   `change ~ arm + baseline`.
#' @noRd
stv_formula_call <- function(spec) {
  response <- if (spec$family == "cox") {
    event <- as.name(spec$event)
    if (isTRUE(spec$event_invert)) event <- call("-", 1, event)
    as.call(list(call("::", as.name("survival"), as.name("Surv")), as.name(spec$time), event))
  } else {
    as.name(spec$outcome)
  }
  terms <- lapply(spec$predictors, as.name)
  if (!is.null(spec$offset) && nzchar(spec$offset)) {
    terms <- c(terms, list(call("offset", call("log", as.name(spec$offset)))))
  }
  rhs <- Reduce(\(a, b) call("+", a, b), terms)
  call("~", response, rhs)
}

#' Printable formula
#'
#' @param spec A model spec.
#'
#' @return A string.
#' @noRd
stv_formula_string <- function(spec) {
  paste(deparse(stv_formula_call(spec), width.cutoff = 500L), collapse = " ")
}

#' Build the plan for a spec
#'
#' @param spec An analysis spec.
#'
#' @return A list with `fun` (name of the stabilitest function), `prep` (list
#'   of expressions) and `call` (the stabilitest call, in terms of `data`).
#' @noRd
stv_plan <- function(spec) {
  s <- spec$settings
  family <- spec$family
  common <- list(
    alpha = s$alpha, n_boot = s$n_boot, max_removal_pct = s$max_removal_pct,
    weights = s$weights, seed = s$seed
  )

  groups <- function() {
    if (stv_is_paired(spec) && identical(spec$paired_layout %||% "wide", "wide")) {
      list(group1 = stv_col(spec$outcome), group2 = stv_col(spec$outcome2))
    } else {
      list(
        group1 = stv_group_values(spec$outcome, spec$group, spec$levels[[1]], spec$success),
        group2 = stv_group_values(spec$outcome, spec$group, spec$levels[[2]], spec$success)
      )
    }
  }

  fun <- stv_families()$stabilitest[stv_families()$id == family]
  args <- switch(
    family,
    location = c(
      groups(), list(test_type = spec$test), common,
      list(influential_threshold = s$influential_threshold, interpret = isTRUE(s$interpret))
    ),
    proportion = c(
      groups(), list(test_type = spec$test), common,
      list(influential_threshold = s$influential_threshold, interpret = isTRUE(s$interpret)),
      if (spec$test %in% c("chisq", "prop")) list(correct = isTRUE(spec$correct))
    ),
    tost = {
      margins <- if (identical(spec$tost_type, "equivalence") && !is.null(spec$delta_L) && !is.null(spec$delta_U)) {
        list(delta_L = spec$delta_L, delta_U = spec$delta_U)
      } else {
        list(margin = spec$margin)
      }
      c(
        groups(),
        list(type = spec$tost_type, endpoint = spec$test),
        margins,
        list(paired = isTRUE(spec$paired), higher_is_better = isTRUE(spec$higher_is_better)),
        common
      )
    },
    lm = c(list(formula = stv_formula_call(spec), data = quote(data), term = spec$term), common),
    glm = c(
      list(
        formula = stv_formula_call(spec), data = quote(data), term = spec$term,
        family = as.call(list(call("::", as.name("stats"), as.name(spec$test))))
      ),
      common,
      if (!is.null(spec$obs_weights) && nzchar(spec$obs_weights)) list(obs_weights = stv_col(spec$obs_weights))
    ),
    cox = c(list(formula = stv_formula_call(spec), data = quote(data), term = spec$term), common)
  )

  list(fun = fun, prep = stv_prep_exprs(spec), call = stv_stabilitest_call(fun, args))
}

#' Evaluate the stabilitest call on prepared data
#'
#' Self-contained so that it can also run in a background process.
#'
#' @param call The `call` of a plan.
#' @param data Prepared data.
#'
#' @return The raw stabilitest result.
#' @noRd
stv_eval_call <- function(call, data) {
  # Seeds give the same numbers only under the same generator. Background
  # workers may use another one, so pin R's default.
  old_kind <- RNGkind()
  on.exit(suppressWarnings(RNGkind(old_kind[[1]], old_kind[[2]], old_kind[[3]])), add = TRUE)
  suppressWarnings(RNGkind("Mersenne-Twister", "Inversion", "Rejection"))
  # Formulas such as `offset(log(x))` look up stats functions from their
  # environment, so evaluate under the global environment.
  env <- new.env(parent = globalenv())
  env$data <- data
  eval(call, env)
}

#' Run a plan in this process
#'
#' @param plan A plan from `stv_plan()`.
#' @param data The filtered data frame.
#'
#' @return A list with `raw` (the stabilitest result), `row_map` and `prepared`.
#' @noRd
stv_run_plan <- function(plan, data) {
  prepared <- stv_apply_prep(plan$prep, data)
  raw <- stv_eval_call(plan$call, prepared$data)
  list(raw = raw, row_map = prepared$row_map, prepared = prepared$data)
}

#' Name of the mirai compute profile used by the app
#'
#' @return A string.
#' @noRd
stv_compute_profile <- function() {
  "stabilitest.viz"
}

#' Start the background workers
#'
#' @param workers Number of daemons.
#'
#' @return `NULL`, invisibly.
#' @noRd
stv_daemons_start <- function(workers) {
  if (workers >= 1) {
    mirai::daemons(workers, .compute = stv_compute_profile())
  }
  invisible(NULL)
}

#' Stop the background workers
#'
#' @return `NULL`, invisibly.
#' @noRd
stv_daemons_stop <- function() {
  try(mirai::daemons(0, .compute = stv_compute_profile()), silent = TRUE)
  invisible(NULL)
}

#' Run a plan in a background process
#'
#' Prepares the data here, so that row numbers are known, then evaluates the
#' stabilitest call in a mirai daemon. Only the prepared data and the call
#' cross the process boundary, so the daemon needs stabilitest but not this
#' package.
#'
#' @param plan A plan from `stv_plan()`.
#' @param data The filtered data frame.
#'
#' @return A list with `task` (a mirai, which resolves to the raw result),
#'   `row_map` and `prepared`.
#' @noRd
stv_run_async <- function(plan, data) {
  prepared <- stv_apply_prep(plan$prep, data)
  runner <- stv_eval_call
  environment(runner) <- globalenv()
  task <- mirai::mirai(
    runner(call, data),
    runner = runner, call = plan$call, data = prepared$data,
    .compute = stv_compute_profile()
  )
  list(task = task, row_map = prepared$row_map, prepared = prepared$data)
}
