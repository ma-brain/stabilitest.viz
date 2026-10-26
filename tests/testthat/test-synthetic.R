test_that("synthetic CSVs ship with the expected shapes", {
  dir <- system.file("extdata", "synthetic", package = "stabilitest.viz")
  skip_if(!nzchar(dir))
  read <- \(f) utils::read.csv(file.path(dir, f))

  expect_identical(dim(read("two_sample_continuous.csv")), c(120L, 3L))
  expect_identical(dim(read("paired_bp.csv")), c(40L, 3L))
  expect_true(all(read("binary_response.csv")$responder %in% 0:1))
  expect_identical(sum(is.na(read("ancova_trial.csv")$change)), 4L)
  expect_true(all(read("adtte.csv")$CNSR %in% 0:1))
  expect_setequal(unique(read("adeff.csv")$AVISIT), c("Baseline", "Week 4", "Week 12"))
  expect_identical(nrow(read("adsl.csv")), 100L)
  expect_identical(dim(read("binary_response_eligible.csv")), c(200L, 3L))
})
