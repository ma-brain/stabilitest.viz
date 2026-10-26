#' File formats the app can read
#'
#' @return A named character vector: file extension to format.
#' @noRd
stv_file_formats <- function() {
  c(
    csv = "csv", tsv = "delim", txt = "delim",
    xlsx = "excel", xls = "excel",
    xpt = "xpt", sas7bdat = "sas7bdat"
  )
}

#' Format of an uploaded file, from its name
#'
#' @param name File name as uploaded (not the temporary path).
#'
#' @return One of the values of `stv_file_formats()`, or `NA`.
#' @noRd
stv_file_format <- function(name) {
  ext <- tolower(sub("^.*\\.([^.]+)$", "\\1", basename(name)))
  unname(stv_file_formats()[ext])
}

#' Why an uploaded file cannot be used, if it cannot
#'
#' @param name File name as uploaded.
#' @param size Size in bytes.
#' @param max_mb Upload limit in megabytes.
#'
#' @return A message, or `NULL` when the file is acceptable.
#' @noRd
stv_file_problem <- function(name, size, max_mb) {
  if (is.na(stv_file_format(name))) {
    return(paste0(
      "Unsupported file type. Use ",
      paste0(".", names(stv_file_formats()), collapse = ", "), "."
    ))
  }
  if (size > max_mb * 1024^2) {
    return(sprintf("File is larger than the %s MB limit.", max_mb))
  }
  NULL
}

#' Sheet names of an Excel file
#'
#' @param path Path to an `.xlsx` or `.xls` file.
#'
#' @return A character vector.
#' @noRd
stv_excel_sheets <- function(path) {
  readxl::excel_sheets(path)
}

#' Read a data file
#'
#' Supports CSV (comma or semicolon), tab-separated text, Excel, SAS transport
#' (`.xpt`) and `.sas7bdat`. SAS variable labels are returned separately so the
#' app can show them as tooltips.
#'
#' @param path Path to the file on disk.
#' @param name Original file name, used to pick the format.
#' @param sheet Excel sheet name or number. Defaults to the first sheet.
#'
#' @return A list with `data` (a tibble), `labels` (a named character vector,
#'   empty strings where a column has no label) and `format`.
#' @noRd
stv_read_file <- function(path, name = basename(path), sheet = NULL) {
  format <- stv_file_format(name)
  if (is.na(format)) {
    rlang::abort(
      paste0("Unsupported file type: ", name),
      class = c("stv_read_error", "error")
    )
  }

  result <- tryCatch(
    switch(
      format,
      csv = , delim = stv_read_delim(path, tab = grepl("\\.tsv$", name, ignore.case = TRUE)),
      excel = list(data = readxl::read_excel(path, sheet = sheet %||% 1), labels = NULL),
      xpt = stv_read_haven(haven::read_xpt(path)),
      sas7bdat = stv_read_haven(haven::read_sas(path))
    ),
    stv_read_error = function(e) stop(e),
    error = function(e) {
      msg <- gsub(path, name, conditionMessage(e), fixed = TRUE)
      rlang::abort(
        paste0("Could not read ", name, ": ", msg),
        class = c("stv_read_error", "error")
      )
    }
  )

  data <- tibble::as_tibble(result$data)
  if (ncol(data) == 0L || nrow(data) == 0L) {
    rlang::abort(
      paste0(name, " has no rows or no columns."),
      class = c("stv_read_error", "error")
    )
  }
  if (anyDuplicated(names(data)) || any(!nzchar(names(data)))) {
    rlang::abort(
      paste0(name, " has empty or duplicated column names."),
      class = c("stv_read_error", "error")
    )
  }

  labels <- result$labels
  if (is.null(labels)) {
    labels <- rlang::set_names(rep("", ncol(data)), names(data))
  }

  list(data = data, labels = labels, format = format)
}

#' Read delimited text with delimiter and decimal mark detection
#'
#' @param path Path to the file.
#' @param tab Force tab as the delimiter.
#'
#' @return A list with `data` and `labels = NULL`.
#' @noRd
stv_read_delim <- function(path, tab = FALSE) {
  first <- readLines(path, n = 1L, warn = FALSE, encoding = "UTF-8")
  first <- sub("^\\ufeff", "", first)
  counts <- c(
    "," = lengths(regmatches(first, gregexpr(",", first, fixed = TRUE))),
    ";" = lengths(regmatches(first, gregexpr(";", first, fixed = TRUE))),
    "\t" = lengths(regmatches(first, gregexpr("\t", first, fixed = TRUE)))
  )
  sep <- if (tab) "\t" else names(counts)[which.max(counts)]

  raw <- utils::read.table(
    path,
    sep = sep, header = TRUE, quote = "\"", comment.char = "",
    colClasses = "character", na.strings = c("", "NA"),
    check.names = FALSE, fill = TRUE, strip.white = TRUE,
    fileEncoding = "UTF-8-BOM"
  )

  # A semicolon often goes with a decimal comma: keep whichever mark yields
  # more numeric columns.
  decimals <- if (identical(sep, ";")) c(".", ",") else "."
  converted <- lapply(decimals, \(dec) {
    lapply(raw, utils::type.convert, as.is = TRUE, dec = dec)
  })
  n_numeric <- vapply(converted, \(cols) sum(vapply(cols, is.numeric, logical(1))), integer(1))
  list(data = tibble::as_tibble(converted[[which.max(n_numeric)]]), labels = NULL)
}

#' Strip haven metadata and collect variable labels
#'
#' @param data A tibble read by haven.
#'
#' @return A list with `data` and `labels`.
#' @noRd
stv_read_haven <- function(data) {
  labels <- vapply(
    data,
    \(x) as.character(attr(x, "label", exact = TRUE) %||% ""),
    character(1)
  )
  data <- data |>
    haven::zap_label() |>
    haven::zap_formats() |>
    haven::zap_labels()
  is_text <- vapply(data, is.character, logical(1))
  data[is_text] <- lapply(data[is_text], haven::zap_empty)
  list(data = data, labels = labels)
}
