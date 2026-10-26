write_text <- function(lines, ext = ".csv", env = parent.frame()) {
  path <- withr_tempfile(ext, env)
  writeLines(lines, path)
  path
}
withr_tempfile <- function(ext, env) {
  path <- tempfile(fileext = ext)
  do.call(on.exit, list(substitute(unlink(path), list(path = path)), add = TRUE), envir = env)
  path
}

test_that("CSV is read with types", {
  path <- write_text(c("id,arm,value", "a,x,1.5", "b,y,2", "c,x,"))
  res <- stv_read_file(path, "d.csv")
  expect_s3_class(res$data, "tbl_df")
  expect_identical(names(res$data), c("id", "arm", "value"))
  expect_type(res$data$value, "double")
  expect_identical(sum(is.na(res$data$value)), 1L)
  expect_true(all(res$labels == ""))
})

test_that("semicolon CSV with decimal commas is read", {
  path <- write_text(c("id;value", "a;1,5", "b;2,25"))
  res <- stv_read_file(path, "d.csv")
  expect_equal(res$data$value, c(1.5, 2.25))
})

test_that("tab-separated text and a UTF-8 BOM are handled", {
  path <- tempfile(fileext = ".tsv")
  on.exit(unlink(path))
  writeBin(c(as.raw(c(0xef, 0xbb, 0xbf)), charToRaw("id\tvalue\na\t1\nb\t2\n")), path)
  res <- stv_read_file(path, "d.tsv")
  expect_identical(names(res$data), c("id", "value"))
  expect_identical(nrow(res$data), 2L)
})

test_that("Excel files are read, with a chosen sheet", {
  path <- readxl::readxl_example("datasets.xlsx")
  expect_true(length(stv_excel_sheets(path)) > 1L)
  res <- stv_read_file(path, "datasets.xlsx", sheet = "mtcars")
  expect_identical(nrow(res$data), 32L)
  expect_true("mpg" %in% names(res$data))
})

test_that("SAS transport files keep labels", {
  df <- data.frame(USUBJID = c("a", "b"), AVAL = c(1, 2))
  attr(df$AVAL, "label") <- "Analysis value"
  path <- tempfile(fileext = ".xpt")
  on.exit(unlink(path))
  haven::write_xpt(df, path)
  res <- stv_read_file(path, "adeff.xpt")
  expect_identical(unname(res$labels[["AVAL"]]), "Analysis value")
  expect_identical(unname(res$labels[["USUBJID"]]), "")
  expect_null(attr(res$data$AVAL, "label"))
})

test_that("sas7bdat files are read", {
  res <- stv_read_file(system.file("examples", "iris.sas7bdat", package = "haven"), "iris.sas7bdat")
  expect_identical(dim(res$data), c(150L, 5L))
})

test_that("bad files fail with a readable error", {
  empty <- write_text(character())
  expect_error(stv_read_file(empty, "e.csv"), class = "stv_read_error")
  header_only <- write_text("a,b")
  expect_error(stv_read_file(header_only, "h.csv"), class = "stv_read_error")
  dup <- write_text(c("a,a", "1,2"))
  expect_error(stv_read_file(dup, "dup.csv"), "duplicated", class = "stv_read_error")
  expect_error(stv_read_file(dup, "x.pdf"), "Unsupported", class = "stv_read_error")
  junk <- write_text("not an excel file", ".xlsx")
  err <- expect_error(stv_read_file(junk, "junk.xlsx"), "Could not read", class = "stv_read_error")
  expect_false(grepl(dirname(junk), conditionMessage(err), fixed = TRUE))
})

test_that("file problems are reported", {
  expect_null(stv_file_problem("a.csv", 100, 50))
  expect_match(stv_file_problem("a.pdf", 100, 50), "Unsupported")
  expect_match(stv_file_problem("a.csv", 51 * 1024^2, 50), "50 MB")
  expect_identical(stv_file_format("A.XLSX"), "excel")
})
