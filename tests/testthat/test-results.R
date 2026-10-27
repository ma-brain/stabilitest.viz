fisher_result <- function(settings) {
  data <- stv_read_file(
    system.file("extdata", "synthetic", "binary_response_eligible.csv", package = "stabilitest.viz"), "x.csv"
  )$data
  spec <- stv_spec_new("proportion", "fisher", outcome = "responder", group = "arm",
    levels = c("Active", "Control"), settings = settings)
  plan <- stv_plan(spec)
  out <- stv_run_plan(plan, data)
  stv_as_result(out$raw, spec, plan, out$prepared, out$row_map)
}

test_that("weights and components shown are the ones stabilitest returned", {
  res <- fisher_result(list(n_boot = 50, seed = 3))
  html <- as.character(stv_results_card(res))
  w <- res@raw$weights
  for (nm in names(w)) {
    expect_match(html, paste0("weight ", round(100 * w[[nm]]), "%"), fixed = TRUE)
  }
  formula <- as.character(stv_score_formula(res))
  expect_match(formula, format(round(res@metrics$worstcase_fragility_component, 1)), fixed = TRUE)
  expect_match(formula, paste("=", format(round(res@score, 1))), fixed = TRUE)
  expect_match(html, "not in score", fixed = TRUE)
})

test_that("without a returned label the score row has no label colour", {
  res <- fisher_result(list(n_boot = 50, seed = 3))
  expect_null(stv_label_tone(res))
  html <- as.character(stv_results_card(res))
  expect_false(grepl("stv-score-bad|stv-score-good|stv-banner-bad|stv-banner-good", html))
  expect_match(html, "No categorical label", fixed = TRUE)
})

test_that("a returned label colours the banner and the composite box only", {
  res <- fisher_result(stv_fisher_profile_settings())
  tone <- stv_label_tone(res)
  expect_false(is.null(tone))
  html <- as.character(stv_results_card(res))
  expect_identical(lengths(regmatches(html, gregexpr(paste0("stv-score-", tone), html))), 1L)
  expect_match(html, paste0("stv-banner-", tone), fixed = TRUE)
  expect_match(html, res@label, fixed = TRUE)
})

test_that("every score box has the same structure", {
  res <- fisher_result(list(n_boot = 50, seed = 3))
  html <- as.character(stv_results_card(res))
  count <- \(pattern) lengths(regmatches(html, gregexpr(pattern, html, fixed = TRUE)))
  expect_identical(count('class="stv-score"') + count('class="stv-score stv-score-head'), 5L)
  for (part in c("stv-score-title", "stv-score-value", "stv-score-caption", "stv-chip")) {
    expect_identical(count(paste0('class="', part)), 5L, info = part)
  }
  expect_identical(count('class="stv-meter"') + count('class="stv-meter-none"'), 5L)
})
