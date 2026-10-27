#' About page UI
#'
#' Package versions and the stabilitest calibration registry.
#'
#' @return A `shiny::tags` div.
#' @noRd
mod_about_ui <- function() {
  shiny::tags$div(
    class = "stv-page",
    bslib::layout_columns(
      col_widths = c(12, 12, 12),
      bslib::card(
        bslib::card_header("Versions"),
        bslib::card_body(
          stv_html_table(stv_versions(), caption = "Package versions"),
          shiny::tags$p(
            class = "small text-body-secondary mt-3 mb-0",
            "stabilitest.viz is a companion to the stabilitest package. It computes",
            "nothing itself: every number comes from stabilitest."
          )
        )
      ),
      bslib::card(
        bslib::card_header("Calibration registry"),
        bslib::card_body(
          shiny::tags$p(
            class = "text-body-secondary",
            "A categorical label is shown only when stabilitest returns one. Methods",
            "marked numeric only show the score and its components without a",
            "validated cutoff."
          ),
          stv_registry_table()
        )
      ),
      bslib::card(
        bslib::card_header("Citation"),
        bslib::card_body(
          shiny::tags$p(class = "text-body-secondary", "If you use stabilitest in published work, please cite it:"),
          shiny::tags$blockquote(class = "stv-citation mb-0", stv_citation_text())
        )
      )
    )
  )
}

#' Citation of the installed stabilitest
#'
#' @return A string.
#' @noRd
stv_citation_text <- function() {
  text <- tryCatch(
    format(utils::citation("stabilitest"), style = "text"),
    error = \(e) character()
  )
  if (!length(text)) {
    return("See https://github.com/ma-brain/stabilitest.")
  }
  # Drop the Markdown emphasis and angle brackets of the text style
  gsub("[_<>]", "", paste(text, collapse = " "))
}

#' Calibration registry table
#'
#' @return A `shiny::tags` object, or an empty state if the registry is missing.
#' @noRd
stv_registry_table <- function() {
  registry <- stv_registry()
  if (is.null(registry)) {
    return(
      stv_empty_state(
        "Registry not available",
        "The installed stabilitest does not ship a calibration registry.",
        icon = "exclamation-circle"
      )
    )
  }

  validated <- registry$status != "uncalibrated"
  display <- tibble::tibble(
    Method = registry$calibration_unit,
    Family = registry$family,
    Endpoint = registry$endpoint,
    Status = lapply(seq_len(nrow(registry)), \(i) {
      shiny::tags$span(
        class = paste("stv-status", if (validated[[i]]) "stv-status-validated"),
        if (validated[[i]]) "validated" else "numeric only"
      )
    }),
    Cutoff = ifelse(
      is.na(registry$cutoff_fragile),
      "no validated cutoff",
      paste0("fragile at \u2264 ", registry$cutoff_fragile)
    ),
    Version = registry$version
  )

  stv_html_table(display, caption = "Calibration registry of stabilitest")
}
