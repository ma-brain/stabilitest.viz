#' Result tables for download
#'
#' The tables stabilitest returned, plus the summary and the removed
#' observations with their data rows.
#'
#' @param result An `stv_result`.
#' @param data The filtered data the result was run on, or `NULL`.
#'
#' @return A named list of data frames.
#' @noRd
stv_export_tables <- function(result, data = NULL) {
  tables <- list(
    summary = stv_result_summary(result),
    metrics = as.data.frame(result@metrics),
    removal_path = as.data.frame(result@removal),
    extreme_path = if (!is.null(result@extreme)) as.data.frame(result@extreme),
    jackknife = as.data.frame(result@jackknife),
    bootstrap = as.data.frame(result@bootstrap),
    removed_observations = if (!is.null(data)) stv_removed_rows(result, data)
  )
  Filter(Negate(is.null), tables)
}

#' Write result tables
#'
#' @param tables Tables from `stv_export_tables()`.
#' @param file Output path.
#' @param format `"xlsx"` (one sheet per table) or `"csv"` (the first table).
#'
#' @return `file`, invisibly.
#' @noRd
stv_write_tables <- function(tables, file, format = c("xlsx", "csv")) {
  format <- rlang::arg_match(format)
  if (format == "xlsx") {
    writexl::write_xlsx(tables, file)
  } else {
    utils::write.csv(tables[[1]], file, row.names = FALSE, na = "")
  }
  invisible(file)
}

#' The plots of a result
#'
#' @param result An `stv_result`.
#' @param mode `"light"` or `"dark"`.
#'
#' @return A named list of ggplots.
#' @noRd
stv_result_plots <- function(result, mode = "light") {
  list(
    removal = stv_plot_removal(result, mode),
    influence = stv_plot_influence(result, mode = mode),
    bootstrap = stv_plot_bootstrap(result, mode)
  )
}

#' Save a plot as PNG or SVG
#'
#' @param plot A ggplot.
#' @param file Output path.
#' @param format `"png"` or `"svg"`.
#'
#' @return `file`, invisibly.
#' @noRd
stv_save_plot <- function(plot, file, format = c("png", "svg")) {
  format <- rlang::arg_match(format)
  if (format == "png") {
    ggplot2::ggsave(file, plot, device = "png", width = 8, height = 4.5, dpi = 200, bg = "white")
  } else {
    ggplot2::ggsave(file, plot, device = svglite::svglite, width = 8, height = 4.5)
  }
  invisible(file)
}

#' Report formats
#'
#' @return A named character vector: Quarto format by file extension.
#' @noRd
stv_report_formats <- function() {
  c(html = "html", docx = "docx", pdf = "typst")
}

#' Whether reports can be rendered here
#'
#' Reports need the Quarto command-line tool.
#'
#' @return `TRUE` or `FALSE`.
#' @noRd
stv_can_render_report <- function() {
  !is.null(quarto::quarto_path())
}

#' A Markdown table
#'
#' @param df A data frame.
#'
#' @return A string.
#' @noRd
stv_md_table <- function(df) {
  esc <- \(x) gsub("|", "\\|", ifelse(is.na(x), "", as.character(x)), fixed = TRUE)
  body <- vapply(seq_len(nrow(df)), \(i) paste0("| ", paste(esc(unlist(df[i, ])), collapse = " | "), " |"), character(1))
  paste(c(
    paste0("| ", paste(esc(names(df)), collapse = " | "), " |"),
    paste0("|", paste(rep("---", ncol(df)), collapse = "|"), "|"),
    body
  ), collapse = "\n")
}

#' Text for the report template
#'
#' Every number comes from the result; the calibration text is the one the
#' app shows, so the report is fail-closed too.
#'
#' @param result An `stv_result`.
#' @param data The filtered data, or `NULL`.
#' @param data_label Name of the data.
#' @param plots Named list of plot file names (`removal`, `influence`,
#'   `bootstrap`), relative to the report.
#'
#' @return A named list of strings, one per template field.
#' @noRd
stv_report_fields <- function(result, data, data_label, plots) {
  m <- result@metrics
  summary <- stv_result_summary(result)
  names(summary) <- c("Measure", "Value")

  removed <- if (!is.null(data)) {
    rows <- stv_removed_rows(result, data)
    rows <- rows[, c("Step", "Observation", "Row", "p after removal", "Decision")]
    paste0("Observations removed on the worst-case path, in order:\n\n", stv_md_table(rows))
  } else {
    ""
  }
  flagged <- result@jackknife[result@jackknife$influential %in% TRUE, , drop = FALSE]
  influential <- if (nrow(flagged)) {
    paste0(
      "stabilitest flags ", nrow(flagged), " observation(s) as influential:\n\n",
      stv_md_table(data.frame(Observation = flagged$label, Row = flagged$row, `p left out` = stv_tooltip_p(flagged$p_value),
        check.names = FALSE))
    )
  } else {
    "stabilitest flags no observation as influential."
  }
  interpretation <- if (!is.null(result@interpretation)) {
    parts <- c(overall = "Overall", jackknife = "Jackknife", worstcase = "Worst-case removal",
      bootstrap = "Bootstrap", recommendation = "Recommendation")
    parts <- parts[names(parts) %in% names(result@interpretation)]
    paste0(
      "## Interpretation\n\nText written by stabilitest for this result.\n\n",
      paste0("**", parts, ".** ", unlist(result@interpretation[names(parts)]), collapse = "\n\n")
    )
  } else {
    ""
  }
  w <- result@weights
  formula <- if (length(w)) {
    paste0(
      "The composite score is the weighted mean of the three components: ",
      paste0(round(100 * w), "% ", c(jackknife = "jackknife stability", fragility = "removal fragility",
        bootstrap = "bootstrap same-decision rate")[names(w)], collapse = ", "),
      ". The original p-value is not part of the score."
    )
  } else {
    ""
  }

  list(
    title = result@method_label,
    date = format(Sys.Date(), "%d %B %Y"),
    data = sprintf("Data: **%s**, %s observations analysed.", data_label %||% "Unnamed data", format(result@n)),
    calibration = stv_calibration_message(result),
    summary = stv_md_table(summary),
    formula = formula,
    removal_intro = paste0(
      "stabilitest removes one observation at a time, each time the one that moves the p-value furthest toward the other decision. ",
      stv_fragility_text(result), "."
    ),
    extreme_caption = if (!is.null(result@extreme)) " (dashed: extreme-value removal, not part of the score)" else "",
    plot_removal = plots$removal,
    removed = removed,
    plot_influence = plots$influence,
    influential = influential,
    bootstrap_intro = sprintf(
      "%s%% of %s valid resamples reach the same decision as the original analysis (the bootstrap same-decision rate). The solid line is the original p-value, the dashed line alpha.",
      stv_format_number(m$bootstrap_same_decision_rate, 0), format(sum(is.finite(result@bootstrap$p_value)))
    ),
    plot_bootstrap = plots$bootstrap,
    interpretation = interpretation,
    code = result@code,
    software = stv_md_table(data.frame(
      Software = c("stabilitest", "stabilitest.viz", "R", "Platform"),
      Version = c(
        as.character(utils::packageVersion("stabilitest")), as.character(utils::packageVersion("stabilitest.viz")),
        paste(R.version$major, R.version$minor, sep = "."), R.version$platform
      )
    ))
  )
}

#' Fill the report template
#'
#' @param fields Named list from `stv_report_fields()`.
#' @param template Path of the template.
#'
#' @return The filled document as a string.
#' @noRd
stv_fill_template <- function(fields, template = app_sys("report", "report.qmd")) {
  text <- paste(readLines(template, warn = FALSE), collapse = "\n")
  for (nm in names(fields)) {
    value <- fields[[nm]]
    # Quoted YAML values must not break on quotes
    if (nm %in% c("title", "date")) value <- gsub("\"", "'", value, fixed = TRUE)
    text <- gsub(paste0("{{", nm, "}}"), value, text, fixed = TRUE)
  }
  text
}

#' Render a report
#'
#' Builds the plots as PNG files, fills the template and renders it with
#' Quarto in a temporary folder.
#'
#' @param result An `stv_result`.
#' @param data The filtered data, or `NULL`.
#' @param data_label Name of the data.
#' @param file Output path.
#' @param format `"html"`, `"docx"` or `"pdf"`.
#'
#' @return `file`, invisibly.
#' @noRd
stv_render_report <- function(result, data, data_label, file, format = c("html", "docx", "pdf")) {
  format <- rlang::arg_match(format)
  dir <- tempfile("stv-report-")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  plots <- stv_result_plots(result)
  paths <- stats::setNames(paste0(names(plots), ".png"), names(plots))
  for (nm in names(plots)) stv_save_plot(plots[[nm]], file.path(dir, paths[[nm]]), "png")

  fields <- stv_report_fields(result, data, data_label, as.list(paths))
  writeLines(stv_fill_template(fields), file.path(dir, "report.qmd"))
  quarto::quarto_render(
    file.path(dir, "report.qmd"),
    output_format = stv_report_formats()[[format]],
    quiet = TRUE
  )
  out <- file.path(dir, paste0("report.", format))
  if (!file.exists(out)) {
    rlang::abort("Quarto did not produce the report.")
  }
  file.copy(out, file, overwrite = TRUE)
  invisible(file)
}
