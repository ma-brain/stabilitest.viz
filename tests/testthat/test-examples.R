test_that("examples have the expected shapes", {
  pain <- stv_load_example("pain_trial")
  expect_s3_class(pain, "tbl_df")
  expect_identical(dim(pain), c(55L, 3L))
  expect_identical(names(pain), c("subject_id", "arm", "change"))
  expect_identical(as.vector(table(pain$arm)), c(28L, 27L))
  expect_equal(pain$change[pain$arm == "Placebo"], stabilitest::pain_placebo)

  ancova <- stv_load_example("pain_ancova_trial")
  expect_s3_class(ancova, "tbl_df")
  expect_identical(dim(ancova), c(80L, 5L))

  onc <- stv_load_example("onc_response_trial")
  expect_s3_class(onc, "tbl_df")
  expect_identical(dim(onc), c(120L, 3L))
  expect_true(all(onc$response %in% 0:1))
})

test_that("unknown examples are rejected", {
  expect_error(stv_load_example("nope"))
})

test_that("column summary reports types and missing values", {
  d <- data.frame(a = c(1, NA), b = factor(c("x", "y")), c = c("u", "v"))
  s <- stv_column_summary(d)
  expect_identical(s$type, c("numeric", "factor", "text"))
  expect_identical(s$n_missing, c(1L, 0L, 0L))
})

test_that("registry and versions are available", {
  reg <- stv_registry()
  expect_true(all(c("calibration_unit", "status", "cutoff_fragile") %in% names(reg)))
  expect_identical(reg$status[reg$calibration_unit == "welch_unpaired"], "uncalibrated")
  expect_identical(stv_versions()$Component[[1]], "stabilitest.viz")
})
