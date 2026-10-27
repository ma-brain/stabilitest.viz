welch_result <- function(interpret = TRUE) {
  data <- stv_read_file(
    system.file("extdata", "synthetic", "two_sample_continuous.csv", package = "stabilitest.viz"), "x.csv"
  )$data
  spec <- stv_spec_new("location", "t.test", outcome = "outcome", group = "arm",
    levels = c("Active", "Control"), settings = list(n_boot = 60, seed = 2, interpret = interpret))
  plan <- stv_plan(spec)
  out <- stv_run_plan(plan, data)
  list(result = stv_as_result(out$raw, spec, plan, out$prepared, out$row_map), data = data)
}

lm_result <- function() {
  data <- datasets::mtcars
  spec <- stv_spec_new("lm", outcome = "mpg", predictors = c("wt", "hp"), term = "wt",
    settings = list(n_boot = 40, seed = 1))
  plan <- stv_plan(spec)
  out <- stv_run_plan(plan, data)
  list(result = stv_as_result(out$raw, spec, plan, out$prepared, out$row_map), data = data)
}

test_that("plots draw the numbers stabilitest returned, in both themes", {
  x <- welch_result()
  r <- x$result
  for (mode in c("light", "dark")) {
    removal <- ggplot2::ggplot_build(stv_plot_removal(r, mode))
    # Worst-case and extreme-value paths, point for point
    pts <- removal$data[[3]]
    expect_equal(sort(pts$y), sort(log10(c(r@removal$p_value, r@extreme$p_value))))
    influence <- ggplot2::ggplot_build(stv_plot_influence(r, selected = r@removed_rows[1:2], mode = mode))
    expect_equal(influence$data[[3]]$y, log10(r@jackknife$p_value))
    expect_equal(nrow(influence$data[[4]]), 2L)
    boot <- ggplot2::ggplot_build(stv_plot_bootstrap(r, mode))
    expect_equal(sum(boot$data[[1]]$count), sum(is.finite(r@bootstrap$p_value) & r@bootstrap$p_value > 0))
    expect_s3_class(stv_plotly(stv_plot_removal(r, mode), mode = mode), "plotly")
  }
})

test_that("plots never use the label colours", {
  r <- welch_result()$result
  forbidden <- toupper(unlist(stv_palette("light")[c("fragile", "not_fragile")]))
  for (mode in c("light", "dark")) {
    forbidden <- c(forbidden, toupper(unlist(stv_palette(mode)[c("fragile", "not_fragile")])))
    for (p in list(stv_plot_removal(r, mode), stv_plot_influence(r, mode = mode), stv_plot_bootstrap(r, mode))) {
      used <- unlist(lapply(ggplot2::ggplot_build(p)$data, \(d) c(d$colour, d$fill)))
      expect_false(any(toupper(used) %in% forbidden))
    }
  }
})

test_that("the removal path marks the step stabilitest reports as the flip", {
  r <- welch_result()$result
  built <- ggplot2::ggplot_build(stv_plot_removal(r))
  ring <- built$data[[5]]
  first_flip <- min(r@removal$k_removed[!r@removal$conclusion_match])
  expect_equal(ring$x, first_flip)
})

test_that("the removed-observations table lists each step with its data row", {
  x <- welch_result()
  r <- x$result
  expect_s3_class(stv_removed_table(r, x$data), "reactable")
  df <- stv_removed_rows(r, x$data)
  steps <- r@removal[r@removal$k_removed > 0, ]
  expect_equal(df$Step, steps$k_removed)
  expect_equal(df$Row, steps$row)
  expect_equal(df$outcome, signif(x$data$outcome[steps$row], 6))
  expect_equal(df$arm, x$data$arm[steps$row])
})

test_that("model results link removal steps to data rows too", {
  x <- lm_result()
  r <- x$result
  steps <- r@removal[r@removal$k_removed > 0, ]
  expect_equal(steps$row, as.integer(r@raw$removed_rows[seq_len(nrow(steps))]))
  expect_s3_class(stv_removed_table(r, x$data), "reactable")
  expect_s3_class(ggplot2::ggplot_build(stv_plot_removal(r)), "ggplot_built")
})

test_that("interpretation shows stabilitest's text, or says how to get it", {
  r <- welch_result(interpret = TRUE)$result
  html <- as.character(stv_interpretation_ui(r))
  expect_match(html, r@interpretation$recommendation, fixed = TRUE)
  none <- welch_result(interpret = FALSE)$result
  expect_match(as.character(stv_interpretation_ui(none)), "Write an interpretation", fixed = TRUE)
  expect_match(as.character(stv_interpretation_ui(lm_result()$result)), "two-group tests only", fixed = TRUE)
})

test_that("the explorer has the six tabs and never says reproducibility", {
  r <- welch_result()$result
  html <- as.character(stv_explorer_ui(shiny::NS("x"), r))
  for (tab in c("Removal path", "Influence", "Bootstrap", "Removed observations", "Interpretation", "Code")) {
    expect_match(html, tab, fixed = TRUE)
  }
  expect_match(html, "bootstrap same-decision rate", fixed = TRUE)
  expect_false(grepl("reproducib", html, ignore.case = TRUE))
})
