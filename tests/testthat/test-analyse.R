# A runner that evaluates in this process and returns an already-resolved task,
# so the module can be tested without background workers.
sync_runner <- function(plan, data) {
  prepared <- stv_apply_prep(plan$prep, data)
  raw <- stv_eval_call(plan$call, prepared$data)
  list(
    task = promises::promise_resolve(raw),
    row_map = prepared$row_map, prepared = prepared$data
  )
}

wait_for <- function(session, check, timeout = 20) {
  start <- Sys.time()
  while (!isTRUE(check()) && difftime(Sys.time(), start, units = "secs") < timeout) {
    later::run_now(0.05)
    session$flushReact()
  }
}

set_fisher_inputs <- function(session, level2 = "Control", ...) {
  session$setInputs(`data-example` = "syn_binary")
  session$setInputs(`question-family` = "proportion", `question-test` = "fisher")
  session$setInputs(
    `map-outcome` = "responder", `map-group` = "arm", `map-level1` = "Active", `map-level2` = level2,
    ...
  )
  session$flushReact()
}

test_that("choosing data and a question builds a spec", {
  store <- stv_store("default")
  shiny::testServer(mod_analyse_server, args = list(store = store), {
    set_fisher_inputs(session)
    spec <- store$spec
    expect_identical(spec$family, "proportion")
    expect_identical(spec$test, "fisher")
    expect_identical(spec$outcome, "responder")
    expect_identical(spec$levels, c("Active", "Control"))
    expect_identical(spec$source$id, "syn_binary")
    expect_identical(spec$settings$n_boot, 1000)
    expect_equal(spec$settings$max_removal_pct, 0.3)
    expect_equal(unname(spec$settings$weights), c(0.4, 0.4, 0.2))
    expect_length(stv_validate_spec(spec, store$data), 0L)
  })
})

test_that("the validated Fisher button sets the profile, in both modes", {
  store <- stv_store("default")
  shiny::testServer(mod_settings_server, args = list(store = store, family = \() list(family = "proportion", test = "fisher")), {
    session$setInputs(alpha = 0.05, n_boot = 500, w_jackknife = 0.4, w_fragility = 0.4, max_removal = 30)
    expect_false(stv_profile_status(list(family = "proportion", test = "fisher", settings = session$returned()))$inside)
    session$setInputs(alpha = 0.05, n_boot = 1000, w_jackknife = 0, w_fragility = 0.5, max_removal = 30)
    expect_true(stv_profile_status(list(family = "proportion", test = "fisher", settings = session$returned()))$inside)
  })
})

test_that("running through the module stores a result and the code", {
  skip_if_not_installed("promises")
  skip_if_not_installed("later")
  store <- stv_store("default")
  shiny::testServer(mod_analyse_server, args = list(store = store, runner = sync_runner), {
    set_fisher_inputs(session, `settings-alpha` = 0.05, `settings-n_boot` = 40, `settings-seed` = 5)
    session$setInputs(`run-run` = 1)
    wait_for(session, \() !is.null(store$result))

    result <- store$result
    expect_true(S7::S7_inherits(result, stv_result))
    expect_identical(result@spec$settings$n_boot, 40)
    expect_match(result@code, "stabilitest::robustness_analysis", fixed = TRUE)
    expect_length(store$history, 1L)
    expect_true(is.na(result@label))
  })
})

test_that("an invalid spec is refused before anything runs", {
  store <- stv_store("default")
  called <- FALSE
  runner <- function(plan, data) {
    called <<- TRUE
    sync_runner(plan, data)
  }
  shiny::testServer(mod_analyse_server, args = list(store = store, runner = runner), {
    set_fisher_inputs(session, level2 = "Active")
    session$setInputs(`run-run` = 1)
    session$flushReact()
    expect_false(called)
    expect_null(store$result)
    expect_match(output[["run-problems"]]$html, "two different groups")
  })
})

test_that("a stabilitest error is shown, not thrown", {
  skip_if_not_installed("promises")
  store <- stv_store("default")
  failing <- function(plan, data) {
    list(task = promises::promise_reject(simpleError("boom")), row_map = integer(), prepared = data)
  }
  shiny::testServer(mod_analyse_server, args = list(store = store, runner = failing), {
    set_fisher_inputs(session)
    # Shiny warns when an ExtendedTask fails; that is expected here
    suppressWarnings({
      session$setInputs(`run-run` = 1)
      wait_for(session, \() grepl("boom", tryCatch(output[["run-status"]]$html, error = \(e) "")))
    })
    expect_match(output[["run-status"]]$html, "stabilitest could not run this analysis: boom")
    expect_null(store$result)
  })
})

test_that("changing the data clears the result and spec", {
  store <- stv_store("default")
  shiny::isolate({
    store$result <- "x"
    stv_store_set_data(store, tibble::tibble(a = 1:12), "t", "t")
    expect_null(store$result)
    expect_identical(store$spec, list())
  })
})
