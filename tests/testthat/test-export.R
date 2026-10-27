export_result <- function(test = "t.test", interpret = TRUE) {
  data <- stv_read_file(
    system.file("extdata", "synthetic", "two_sample_continuous.csv", package = "stabilitest.viz"), "x.csv"
  )$data
  spec <- stv_spec_new("location", test, outcome = "outcome", group = "arm",
    levels = c("Active", "Control"), settings = list(n_boot = 40, seed = 4, interpret = interpret))
  plan <- stv_plan(spec)
  out <- stv_run_plan(plan, data)
  list(result = stv_as_result(out$raw, spec, plan, out$prepared, out$row_map), data = data)
}

test_that("exported tables are the tables of the result", {
  x <- export_result()
  tables <- stv_export_tables(x$result, x$data)
  expect_named(tables, c("summary", "metrics", "removal_path", "extreme_path", "jackknife", "bootstrap",
    "removed_observations"))
  expect_equal(tables$jackknife$p_value, x$result@jackknife$p_value)
  expect_equal(tables$bootstrap$p_value, x$result@bootstrap$p_value)

  xlsx <- tempfile(fileext = ".xlsx")
  stv_write_tables(tables, xlsx, "xlsx")
  expect_identical(readxl::excel_sheets(xlsx), names(tables))
  back <- readxl::read_xlsx(xlsx, sheet = "removal_path")
  expect_equal(back$p_value, x$result@removal$p_value)

  csv <- tempfile(fileext = ".csv")
  stv_write_tables(tables["jackknife"], csv, "csv")
  expect_equal(utils::read.csv(csv)$p_value, x$result@jackknife$p_value)
})

test_that("plots save as PNG and SVG", {
  r <- export_result()$result
  for (fmt in c("png", "svg")) {
    f <- tempfile(fileext = paste0(".", fmt))
    stv_save_plot(stv_plot_removal(r), f, fmt)
    expect_gt(file.size(f), 1000)
  }
})

test_that("report text is fail-closed and uses stabilitest's numbers", {
  x <- export_result()
  fields <- stv_report_fields(x$result, x$data, "Two-sample", list(removal = "a.png", influence = "b.png",
    bootstrap = "c.png"))
  expect_identical(fields$calibration, stv_calibration_message(x$result))
  expect_match(fields$summary, stv_format_p(x$result@p_value), fixed = TRUE)
  expect_match(fields$interpretation, x$result@interpretation$recommendation, fixed = TRUE)
  doc <- stv_fill_template(fields)
  expect_false(grepl("{{", doc, fixed = TRUE))
  expect_false(grepl("reproducib", gsub("Reproducible code", "", doc), ignore.case = TRUE))
  none <- export_result(interpret = FALSE)$result
  expect_identical(stv_report_fields(none, NULL, NULL, list())$interpretation, "")
})

test_that("reports render in all three formats", {
  skip_on_cran()
  skip_if_not(stv_can_render_report(), "Quarto is not installed")
  x <- export_result()
  for (fmt in c("html", "docx", "pdf")) {
    f <- tempfile(fileext = paste0(".", fmt))
    stv_render_report(x$result, x$data, "Two-sample", f, fmt)
    expect_gt(file.size(f), 10000)
  }
  html <- tempfile(fileext = ".html")
  stv_render_report(x$result, x$data, "Two-sample", html, "html")
  text <- paste(readLines(html, warn = FALSE), collapse = "\n")
  expect_match(text, "Numeric only, no validated cutoff", fixed = TRUE)
})

test_that("downloads have descriptive names", {
  r <- export_result()$result
  expect_match(stv_download_name(r, "report", "pdf"), "^stabilitest-[a-z0-9-]+-report-\\d{8}\\.pdf$")
})
