run_result <- function(test, settings = list(n_boot = 40, seed = 4)) {
  data <- stv_read_file(
    system.file("extdata", "synthetic", "two_sample_continuous.csv", package = "stabilitest.viz"), "x.csv"
  )$data
  spec <- stv_spec_new("location", test, outcome = "outcome", group = "arm",
    levels = c("Active", "Control"), settings = settings)
  plan <- stv_plan(spec)
  out <- stv_run_plan(plan, data)
  list(result = stv_as_result(out$raw, spec, plan, out$prepared, out$row_map), data = data)
}

history_with <- function(...) {
  store <- stv_store("default")
  shiny::isolate({
    x <- run_result("t.test")
    stv_store_set_data(store, x$data, "Two-sample continuous", "syn_two_sample")
    for (test in c(...)) stv_store_add_result(store, run_result(test)$result)
  })
  store
}

test_that("history entries record the result with its data", {
  store <- history_with("t.test", "wilcoxon")
  h <- shiny::isolate(store$history)
  expect_identical(vapply(h, \(e) e$id, integer(1)), 1:2)
  expect_identical(h[[1]]$data_label, "Two-sample continuous")
  expect_identical(h[[2]]$n_rows, 120L)
  shiny::isolate(stv_store_remove_results(store, 1L))
  h <- shiny::isolate(store$history)
  expect_length(h, 1L)
  shiny::isolate(stv_store_add_result(store, h[[1]]$result))
  # Ids keep counting after a removal
  expect_identical(vapply(shiny::isolate(store$history), \(e) e$id, integer(1)), 2:3)
})

test_that("the compare table shows each result's own summary", {
  h <- shiny::isolate(history_with("t.test", "wilcoxon")$history)
  tbl <- stv_compare_table(h)
  expect_identical(names(tbl), c("Measure", "#1", "#2"))
  for (i in 1:2) {
    expect_identical(tbl[[i + 1]][-1], stv_result_summary(h[[i]]$result)$value)
  }
  expect_true(stv_same_data(h))
  label_row <- tbl[tbl$Measure == "Label", ]
  expect_identical(unname(unlist(label_row[-1])), rep("None (numeric only, no validated cutoff)", 2))
  h[[2]]$n_rows <- 60L
  expect_false(stv_same_data(h))
})

test_that("summary numbers are the ones stabilitest returned", {
  r <- run_result("t.test")$result
  s <- stats::setNames(stv_result_summary(r)$value, stv_result_summary(r)$measure)
  expect_identical(s[["Original p-value"]], stv_format_p(r@raw$original_p))
  expect_match(s[["Composite score"]], stv_format_number(r@raw$metrics$overall_robustness, 0), fixed = TRUE)
  expect_match(s[["Bootstrap same-decision rate"]],
    stv_format_number(r@raw$metrics$bootstrap_reproducibility, 0), fixed = TRUE)
  expect_false(any(grepl("reproducib", s, ignore.case = TRUE)))
})

test_that("the compare plot overlays each removal path without label colours", {
  h <- shiny::isolate(history_with("t.test", "wilcoxon", "brunner_munzel")$history)
  for (mode in c("light", "dark")) {
    built <- ggplot2::ggplot_build(stv_plot_compare_removal(h, mode))
    lines <- built$data[[2]]
    expect_identical(length(unique(lines$group)), 3L)
    expect_equal(sort(lines$y), sort(log10(unlist(lapply(h, \(e) e$result@removal$p_value)))))
    forbidden <- toupper(unlist(stv_palette(mode)[c("fragile", "not_fragile")]))
    expect_false(any(toupper(lines$colour) %in% forbidden))
    expect_false(any(toupper(lines$colour) == toupper(stv_palette(mode)$surface)))
  }
})

test_that("the compare page lists the history and compares the selection", {
  store <- history_with("t.test", "wilcoxon")
  shiny::testServer(mod_compare_server, args = list(store = store), {
    session$setInputs(history__reactable__selected = c(1L, 2L))
    expect_length(chosen(), 2L)
    session$setInputs(remove = 1)
    expect_length(store$history, 0L)
  })
})
