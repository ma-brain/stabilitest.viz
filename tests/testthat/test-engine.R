syn <- \(f) stv_read_file(system.file("extdata", "synthetic", f, package = "stabilitest.viz"), f)$data
fast <- list(n_boot = 30, seed = 7)

specs <- function() {
  list(
    welch = stv_spec_new("location", "t.test", outcome = "outcome", group = "arm",
      levels = c("Active", "Control"), settings = fast),
    wilcoxon_paired_wide = stv_spec_new("location", "paired.t.test", outcome = "before",
      outcome2 = "after", settings = fast),
    fisher = stv_spec_new("proportion", "fisher", outcome = "responder", group = "arm",
      levels = c("Active", "Control"), settings = fast),
    tost_mean = stv_spec_new("tost", "mean", tost_type = "equivalence", margin = 8, outcome = "outcome",
      group = "arm", levels = c("Active", "Control"), settings = fast),
    tost_or = stv_spec_new("tost", "or", tost_type = "noninferiority", margin = 1.25, higher_is_better = TRUE,
      outcome = "responder", group = "arm", levels = c("Active", "Control"), settings = fast),
    lm = stv_spec_new("lm", outcome = "change", predictors = c("arm", "baseline"), term = "armLow",
      reference = list(arm = "Placebo"), settings = fast),
    glm = stv_spec_new("glm", "poisson", outcome = "events", predictors = "arm", offset = "exposure_years",
      term = "armControl", settings = list(n_boot = 20, seed = 7)),
    cox = stv_spec_new("cox", time = "time_months", event = "event", predictors = "arm", term = "armControl",
      settings = list(n_boot = 20, seed = 7))
  )
}
data_for <- c(
  welch = "two_sample_continuous.csv", wilcoxon_paired_wide = "paired_bp.csv", fisher = "binary_response.csv",
  tost_mean = "two_sample_continuous.csv", tost_or = "binary_response.csv", lm = "ancova_trial.csv",
  glm = "count_events.csv", cox = "survival_cox.csv"
)

test_that("every family runs and normalises to the same result shape", {
  skip_if_not_installed("survival")
  for (nm in names(specs())) {
    data <- syn(data_for[[nm]])
    spec <- stv_spec_finalize(specs()[[nm]], data)
    plan <- stv_plan(spec)
    out <- stv_run_plan(plan, data)
    res <- stv_as_result(out$raw, spec, plan, out$prepared, out$row_map)

    expect_true(S7::S7_inherits(res, stv_result), info = nm)
    expect_identical(res@family, spec$family, info = nm)
    expect_true(is.finite(res@p_value) && is.finite(res@score), info = nm)
    expect_true("bootstrap_same_decision_rate" %in% names(res@metrics), info = nm)
    expect_false("bootstrap_reproducibility" %in% names(res@metrics), info = nm)
    expect_identical(
      names(res@jackknife),
      c("unit", "label", "row", "p_value", "statistic", "estimate", "significant",
        "conclusion_match", "influential_delta", "influential"), info = nm
    )
    expect_identical(res@removal$k_removed[[1]], 0L, info = nm)
    expect_identical(nrow(res@bootstrap) > 0L, TRUE, info = nm)
    expect_true(all(res@jackknife$row %in% seq_len(nrow(data))), info = nm)
    expect_true(is.na(res@label), info = nm)
  }
})

test_that("rows link back to the filtered data after complete-case selection", {
  data <- syn("ancova_trial.csv")
  spec <- stv_spec_finalize(specs()$lm, data)
  plan <- stv_plan(spec)
  out <- stv_run_plan(plan, data)
  res <- stv_as_result(out$raw, spec, plan, out$prepared, out$row_map)

  expect_identical(nrow(out$prepared), 86L)
  expect_identical(res@n, 86L)
  expect_false(any(is.na(data$change[res@jackknife$row])))
  expect_true(all(res@removed_rows %in% which(!is.na(data$change))))
  expect_identical(length(res@removed_rows), length(out$raw$removed_rows))
})

test_that("analysis units map to rows of the right group", {
  data <- syn("two_sample_continuous.csv")
  spec <- specs()$welch
  plan <- stv_plan(spec)
  out <- stv_run_plan(plan, data)
  res <- stv_as_result(out$raw, spec, plan, out$prepared, out$row_map)
  g1 <- res@jackknife[startsWith(res@jackknife$label, "G1-"), ]
  expect_true(all(data$arm[g1$row] == "Active"))
  g2 <- res@jackknife[startsWith(res@jackknife$label, "G2-"), ]
  expect_true(all(data$arm[g2$row] == "Control"))
  expect_identical(nrow(res@jackknife), nrow(data))
})

test_that("generated code reproduces the app result exactly", {
  skip_if_not_installed("survival")
  for (nm in names(specs())) {
    data <- syn(data_for[[nm]])
    spec <- stv_spec_finalize(specs()[[nm]], data)
    plan <- stv_plan(spec)
    app <- stv_run_plan(plan, data)$raw

    code <- stv_codegen(spec)
    env <- new.env(parent = globalenv())
    env$data <- data
    utils::capture.output(eval(parse(text = code), env))
    expect_identical(env$result, app, info = nm)
  }
})

test_that("generated code includes the data loading and filters", {
  spec <- specs()$fisher
  spec$source <- list(id = "syn_adeff", filters = list(paramcd = "PAIN", avisit = "Week 12", flags = "ANL01FL"),
    adam = stv_detect_adam(syn("adeff.csv")))
  code <- stv_codegen(spec)
  expect_match(code, "read.csv(path", fixed = TRUE)
  expect_match(code, "PARAMCD", fixed = TRUE)
  expect_match(code, "Week 12", fixed = TRUE)
  expect_match(code, "ANL01FL", fixed = TRUE)
  expect_match(code, "stabilitest::robustness_analysis(", fixed = TRUE)
  expect_match(code, "seed = 7", fixed = TRUE)

  up <- specs()$lm
  up$source <- list(file = "mine.xlsx", format = "excel", sheet = "Data")
  expect_match(stv_codegen(up), "readxl::read_excel(\"mine.xlsx\", sheet = \"Data\")", fixed = TRUE)
})

test_that("the pipeline from filtered ADaM data to code is reproducible", {
  d <- syn("adeff.csv")
  adam <- stv_detect_adam(d)
  filters <- stv_default_filters(d, adam)
  filtered <- stv_apply_filters(d, adam, filters)
  spec <- stv_spec_finalize(
    stv_spec_new("lm", outcome = "CHG", predictors = c("TRTP", "BASE"), term = "TRTPPlacebo", settings = fast),
    filtered
  )
  spec$source <- list(id = "syn_adeff", filters = filters, adam = adam)
  app <- stv_run_plan(stv_plan(spec), filtered)$raw

  code <- stv_codegen(spec)
  env <- new.env(parent = globalenv())
  utils::capture.output(eval(parse(text = code), env))
  expect_identical(env$result, app)
})

test_that("a validated Fisher label is passed through, and only then", {
  data <- syn("binary_response_eligible.csv")
  spec <- stv_spec_new("proportion", "fisher", outcome = "responder", group = "arm",
    levels = c("Active", "Control"), settings = stv_fisher_profile_settings())
  plan <- stv_plan(spec)
  out <- stv_run_plan(plan, data)
  res <- stv_as_result(out$raw, spec, plan, out$prepared, out$row_map)
  expect_true(res@calibration$applicable)
  expect_identical(res@label, out$raw$interpretation_label)
  expect_false(is.na(res@label))
  expect_identical(stv_calibration_state(res), "validated_label")

  # Same data, default weights: outside the validated profile, so numeric only
  spec2 <- stv_spec_new("proportion", "fisher", outcome = "responder", group = "arm",
    levels = c("Active", "Control"))
  out2 <- stv_run_plan(stv_plan(spec2), data)
  res2 <- stv_as_result(out2$raw, spec2, stv_plan(spec2), out2$prepared, out2$row_map)
  expect_true(is.na(res2@label))
  expect_identical(stv_calibration_state(res2), "numeric_only")
  expect_match(stv_calibration_message(res2), "Numeric only")
})

test_that("a label cannot be set without an applicable calibration", {
  data <- syn("two_sample_continuous.csv")
  spec <- specs()$welch
  plan <- stv_plan(spec)
  out <- stv_run_plan(plan, data)
  res <- stv_as_result(out$raw, spec, plan, out$prepared, out$row_map)
  expect_error(S7::prop(res, "label") <- "Fragile", "applicable")
})

test_that("the app's validated Fisher settings match stabilitest's frozen design", {
  expect_equal(stv_fisher_profile_settings()$weights, stabilitest:::.fisher_exact_calibration_weights)
})

test_that("profile status only reports on settings", {
  data_spec <- stv_spec_new("proportion", "fisher", settings = stv_fisher_profile_settings())
  expect_true(stv_profile_status(data_spec)$inside)
  expect_false(stv_profile_status(stv_spec_new("proportion", "fisher"))$inside)
  expect_false(stv_profile_status(stv_spec_new("location", "t.test"))$has_profile)
})

test_that("results from a background daemon equal a local run", {
  skip_on_cran()
  skip_if_not_installed("mirai")
  mirai::daemons(1, .compute = stv_compute_profile())
  on.exit(stv_daemons_stop())

  data <- syn("ancova_trial.csv")
  spec <- stv_spec_finalize(specs()$lm, data)
  plan <- stv_plan(spec)
  local <- stv_run_plan(plan, data)
  async <- stv_run_async(plan, data)
  remote <- mirai::call_mirai(async$task)$data
  expect_false(mirai::is_error_value(remote))
  expect_identical(remote, local$raw)
  expect_identical(async$row_map, local$row_map)
})

test_that("a cancelled background run reports an error value", {
  skip_on_cran()
  mirai::daemons(1, .compute = stv_compute_profile())
  on.exit(stv_daemons_stop())

  data <- syn("two_sample_continuous.csv")
  spec <- stv_spec_new("location", "wilcoxon", outcome = "outcome", group = "arm",
    levels = c("Active", "Control"), settings = list(n_boot = 2000, seed = 1))
  async <- stv_run_async(stv_plan(spec), data)
  mirai::stop_mirai(async$task)
  expect_true(mirai::is_error_value(mirai::call_mirai(async$task)$data))
})
