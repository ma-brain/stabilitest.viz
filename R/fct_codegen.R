#' Print a call with one argument per line
#'
#' @param call A call with named arguments.
#'
#' @return A character vector of lines.
#' @noRd
stv_format_call <- function(call) {
  fun <- paste(deparse(call[[1]], width.cutoff = 500L), collapse = "")
  args <- as.list(call)[-1]
  lines <- vapply(seq_along(args), \(i) {
    value <- paste(deparse(args[[i]], width.cutoff = 70L), collapse = "\n  ")
    nm <- names(args)[[i]]
    sep <- if (i < length(args)) "," else ""
    paste0("  ", if (nzchar(nm)) paste0(nm, " = "), value, sep)
  }, character(1))
  c(paste0(fun, "("), lines, ")")
}

#' Code that loads the data and applies the Data step filters
#'
#' @param source A list describing where the data came from: `id` (bundled
#'   example id), `file` and `format` and `sheet` for uploads, `filters` and
#'   `adam` (as in the store). `NULL` gives a placeholder.
#'
#' @return A character vector of lines.
#' @noRd
stv_data_code <- function(source) {
  if (is.null(source)) {
    return("# data <- your data frame (after any filtering)")
  }

  load <- if (!is.null(source$id)) {
    examples <- stv_examples()
    file <- examples$file[examples$id == source$id]
    if (!is.na(file)) {
      c(
        sprintf(
          "path <- system.file(\"extdata\", \"synthetic\", %s, package = \"stabilitest.viz\")",
          deparse(file)
        ),
        "data <- utils::read.csv(path, stringsAsFactors = FALSE)"
      )
    } else if (source$id == "pain_trial") {
      c(
        "data <- data.frame(",
        "  arm = rep(c(\"Treatment\", \"Placebo\"), c(length(stabilitest::pain_treatment), length(stabilitest::pain_placebo))),",
        "  change = c(stabilitest::pain_treatment, stabilitest::pain_placebo)",
        ")"
      )
    } else {
      paste0("data <- stabilitest::", source$id)
    }
  } else {
    file <- deparse(source$file %||% "your_file")
    switch(
      source$format %||% "csv",
      csv = , delim = paste0("data <- utils::read.csv(", file, ", na.strings = c(\"\", \"NA\"), check.names = FALSE)"),
      excel = paste0("data <- readxl::read_excel(", file, if (!is.null(source$sheet)) paste0(", sheet = ", deparse(source$sheet)), ")"),
      xpt = paste0("data <- haven::zap_labels(haven::read_xpt(", file, "))"),
      sas7bdat = paste0("data <- haven::zap_labels(haven::read_sas(", file, "))"),
      paste0("data <- ", file)
    )
  }

  c(load, stv_filter_code(source$filters, source$adam))
}

#' Code for the Data step filters
#'
#' Mirrors `stv_apply_filters()`.
#'
#' @param filters A filter list, or `NULL`.
#' @param adam Result of `stv_detect_adam()`.
#'
#' @return A character vector of lines.
#' @noRd
stv_filter_code <- function(filters, adam) {
  if (is.null(filters)) {
    return(character())
  }
  v <- adam$vars
  lines <- character()
  keep <- function(condition) {
    lines <<- c(lines, sprintf("data <- data[%s, , drop = FALSE]", condition))
  }
  equals <- function(col, value) {
    sprintf("as.character(data[[%s]]) %%in%% %s", deparse(col), deparse(value))
  }

  if (!is.null(v$paramcd) && length(filters$paramcd) && nzchar(filters$paramcd)) {
    keep(equals(v$paramcd, filters$paramcd))
  }
  if (!is.null(v$avisit) && length(filters$avisit) && nzchar(filters$avisit)) {
    keep(equals(v$avisit, filters$avisit))
  }
  for (flag in filters$flags) {
    keep(sprintf("toupper(as.character(data[[%s]])) %%in%% \"Y\"", deparse(flag)))
  }
  if (length(filters$row_col) == 1L && nzchar(filters$row_col) && length(filters$row_vals)) {
    keep(equals(filters$row_col, filters$row_vals))
  }
  lines
}

#' Reproducible R code for an analysis
#'
#' Prints the same plan that the app evaluates, preceded by the code that loads
#' and filters the data.
#'
#' @param spec An analysis spec, optionally with `source`.
#'
#' @return A single string.
#' @noRd
stv_codegen <- function(spec) {
  plan <- stv_plan(spec)
  prep <- unlist(lapply(plan$prep, \(step) {
    if (identical(step, quote(data <- as.data.frame(data)))) {
      "data <- as.data.frame(data)"
    } else {
      deparse(step, width.cutoff = 80L)
    }
  }))

  header <- c(
    "# Reproduces the analysis run in stabilitest.viz.",
    sprintf(
      "# stabilitest %s, R %s.",
      as.character(getNamespaceVersion("stabilitest")),
      paste(R.version$major, R.version$minor, sep = ".")
    ),
    "# Every number comes from stabilitest; nothing is computed by the app.",
    ""
  )

  paste(
    c(
      header,
      stv_data_code(spec$source),
      "",
      "# Seeds give the same numbers only under R's default random number generator",
      "RNGkind(\"Mersenne-Twister\", \"Inversion\", \"Rejection\")",
      "",
      "# Prepare: complete cases on the columns used",
      prep,
      "",
      "# Analyse",
      paste0("result <- ", stv_format_call(plan$call)[[1]]),
      stv_format_call(plan$call)[-1],
      "",
      "print(result)"
    ),
    collapse = "\n"
  )
}
