syn <- \(f) stv_read_file(system.file("extdata", "synthetic", f, package = "stabilitest.viz"), f)$data

test_that("ADaM structure is detected from column names", {
  expect_identical(stv_detect_adam(syn("adsl.csv"))$kind, "ADSL")
  expect_identical(stv_detect_adam(syn("adeff.csv"))$kind, "BDS")
  expect_identical(stv_detect_adam(syn("adtte.csv"))$kind, "ADTTE")
  expect_true(is.na(stv_detect_adam(syn("two_sample_continuous.csv"))$kind))

  lower <- syn("adeff.csv")
  names(lower) <- tolower(names(lower))
  expect_identical(stv_detect_adam(lower)$kind, "BDS")
  expect_identical(stv_detect_adam(lower)$vars$aval, "aval")
})

test_that("flag columns are found", {
  expect_setequal(stv_detect_adam(syn("adsl.csv"))$vars$flags, c("SAFFL", "ITTFL", "EFFFL"))
  expect_true("ANL01FL" %in% stv_detect_adam(syn("adeff.csv"))$vars$flags)
})

test_that("default filters give one row per subject for BDS", {
  d <- syn("adeff.csv")
  adam <- stv_detect_adam(d)
  filters <- stv_default_filters(d, adam)
  expect_identical(filters$paramcd, "PAIN")
  expect_identical(filters$avisit, "Week 12")
  expect_identical(filters$flags, "ANL01FL")

  filtered <- stv_apply_filters(d, adam, filters)
  expect_identical(nrow(filtered), 100L)
  checks <- stv_check_subjects(filtered, adam)
  expect_true(checks$ok)
  expect_identical(checks$n_subjects, 100L)

  all_visits <- stv_apply_filters(d, adam, modifyList(filters, list(avisit = "")))
  checks <- stv_check_subjects(all_visits, adam)
  expect_false(checks$ok)
  expect_identical(checks$n_repeated, 100L)
})

test_that("population flags and row filters apply, stale values are ignored", {
  d <- syn("adsl.csv")
  adam <- stv_detect_adam(d)
  none <- list(paramcd = NULL, avisit = NULL, flags = character(), row_col = NULL, row_vals = NULL)
  expect_identical(nrow(stv_apply_filters(d, adam, modifyList(none, list(flags = "ITTFL")))), 97L)
  expect_identical(nrow(stv_apply_filters(d, adam, modifyList(none, list(row_col = "SEX", row_vals = "F")))), sum(d$SEX == "F"))
  expect_identical(nrow(stv_apply_filters(d, adam, modifyList(none, list(flags = "NOPE")))), 100L)

  bds <- syn("adeff.csv")
  expect_identical(
    nrow(stv_apply_filters(bds, stv_detect_adam(bds), modifyList(none, list(paramcd = "STALE")))),
    nrow(bds)
  )
})

test_that("ADaM presets map the standard variables", {
  bds <- syn("adeff.csv")
  s <- stv_suggest_mapping(bds, stv_detect_adam(bds))
  expect_identical(s$column[s$role == "Subject"], "USUBJID")
  expect_identical(s$column[s$role == "Group"], "TRTP")
  expect_identical(s$column[s$role == "Outcome"], "CHG")
  expect_identical(s$column[s$role == "Covariate"], "BASE")

  tte <- syn("adtte.csv")
  s <- stv_suggest_mapping(tte, stv_detect_adam(tte))
  expect_identical(s$column[s$role == "Time"], "AVAL")
  expect_identical(s$column[s$role == "Event"], "CNSR")
  expect_match(s$note[s$role == "Event"], "1 - CNSR")
})

test_that("non-ADaM data get heuristic suggestions", {
  guess <- \(f) {
    d <- syn(f)
    s <- stv_suggest_mapping(d, stv_detect_adam(d))
    rlang::set_names(s$column, s$role)
  }
  two <- guess("two_sample_continuous.csv")
  expect_identical(two[["Group"]], "arm")
  expect_identical(two[["Outcome"]], "outcome")

  expect_identical(guess("binary_response.csv")[["Outcome"]], "responder")

  anc <- guess("ancova_trial.csv")
  expect_identical(anc[["Group"]], "arm")
  expect_identical(anc[["Outcome"]], "change")
  expect_setequal(unname(anc[names(anc) == "Covariate"]), c("age", "baseline"))

  surv <- guess("survival_cox.csv")
  expect_identical(surv[["Time"]], "time_months")
  expect_identical(surv[["Event"]], "event")
  expect_false("Outcome" %in% names(surv))

  expect_identical(guess("count_events.csv")[["Outcome"]], "events")
})
