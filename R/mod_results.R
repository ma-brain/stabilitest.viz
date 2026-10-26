#' Results area
#'
#' M2 shows the verdict numbers, the calibration banner and the reproducible
#' code. The tabbed explorer (removal path, influence, bootstrap and so on) is
#' M3.
#'
#' @param id Module id.
#'
#' @return A `shiny::uiOutput()`.
#' @noRd
mod_results_ui <- function(id) {
  shiny::uiOutput(shiny::NS(id, "results"))
}

#' Results server
#'
#' @param id Module id.
#' @param store Session store from `stv_store()`.
#'
#' @return `NULL`, invisibly.
#' @noRd
mod_results_server <- function(id, store) {
  shiny::moduleServer(id, function(input, output, session) {
    output$results <- shiny::renderUI({
      result <- store$result
      if (is.null(result)) {
        return(mod_results_placeholder_ui())
      }
      stv_results_card(result)
    })
    invisible(NULL)
  })
}

#' Format a p-value
#'
#' @param p A number.
#'
#' @return A string.
#' @noRd
stv_format_p <- function(p) {
  if (is.na(p)) "NA" else if (p < 0.001) "<0.001" else format(signif(p, 3), scientific = FALSE)
}

#' Banner stating whether stabilitest returned a validated label
#'
#' Neutral unless stabilitest returned a label. Only then are green and red
#' used.
#'
#' @param result An `stv_result`.
#'
#' @return A `shiny::tags` div.
#' @noRd
stv_calibration_banner <- function(result) {
  if (identical(stv_calibration_state(result), "numeric_only")) {
    return(
      shiny::tags$div(
        class = "stv-banner stv-banner-numeric", role = "note",
        bsicons::bs_icon("info-circle", a11y = "deco"),
        shiny::tags$div(
          shiny::tags$div(class = "stv-banner-title", "No categorical label"),
          shiny::tags$div(stv_calibration_message(result))
        )
      )
    )
  }
  label <- result@label
  tone <- if (grepl("not fragile|^robust", label, ignore.case = TRUE)) {
    "good"
  } else if (grepl("fragile", label, ignore.case = TRUE)) {
    "bad"
  } else {
    "neutral"
  }
  shiny::tags$div(
    class = paste0("stv-banner stv-banner-", tone), role = "note",
    bsicons::bs_icon("patch-check", a11y = "deco"),
    shiny::tags$div(
      shiny::tags$div(class = "stv-banner-title", label),
      shiny::tags$div("Label returned by stabilitest under its validated calibration profile.")
    )
  )
}

#' Results card
#'
#' @param result An `stv_result`.
#'
#' @return A `bslib::card()`.
#' @noRd
stv_results_card <- function(result) {
  m <- result@metrics
  tost <- identical(result@family, "tost")
  decision <- if (tost) {
    if (isTRUE(result@significant)) "Claim shown at this level" else "Claim not shown at this level"
  } else if (isTRUE(result@significant)) {
    sprintf("Significant at %s", format(result@alpha))
  } else {
    sprintf("Not significant at %s", format(result@alpha))
  }
  number <- \(x, digits = 1) if (length(x) && is.finite(x)) format(round(x, digits), nsmall = digits) else "n/a"

  box <- function(title, value, detail = NULL) {
    bslib::value_box(
      title = title, value = value, detail,
      class = "stv-value-box", showcase = NULL, fill = FALSE
    )
  }
  estimate <- if (is.finite(result@estimate)) {
    paste0(
      "Estimate ", format(signif(result@estimate, 4)),
      if (all(is.finite(result@ci))) paste0(" (", paste(format(signif(result@ci, 4)), collapse = " to "), ")")
    )
  }

  bslib::card(
    bslib::card_header(
      shiny::tags$span(result@method_label),
      shiny::tags$span(class = "stv-type ms-auto", paste("n =", result@n))
    ),
    bslib::card_body(
      stv_calibration_banner(result),
      bslib::layout_column_wrap(
        width = "10rem", fill = FALSE,
        box("Original p-value", stv_format_p(result@p_value), shiny::tags$small(decision)),
        box("Composite score", number(result@score, 0), shiny::tags$small("0 to 100, numeric")),
        box(
          "Removal fragility index", number(m$worstcase_fragility_k, 0),
          shiny::tags$small(paste0(number(m$worstcase_fragility_pct), "% of observations"))
        ),
        box(
          "Bootstrap same-decision rate", paste0(number(m$bootstrap_same_decision_rate, 0), "%"),
          shiny::tags$small("of resamples keep the decision")
        ),
        box("Jackknife stability", paste0(number(m$jackknife_conclusion_stability, 0), "%"), shiny::tags$small("leave-one-out"))
      ),
      if (!is.null(estimate)) shiny::tags$p(class = "stv-num text-body-secondary mb-0", estimate),
      shiny::tags$div(
        class = "stv-code-wrap",
        shiny::tags$div(
          class = "d-flex align-items-center mb-2",
          shiny::tags$h3(class = "stv-section-title mb-0", "Reproducible code"),
          shiny::tags$button(
            type = "button", class = "btn btn-sm btn-outline-secondary ms-auto",
            onclick = "navigator.clipboard.writeText(this.closest('.stv-code-wrap').querySelector('code').innerText)",
            bsicons::bs_icon("clipboard", a11y = "deco"), " Copy"
          )
        ),
        shiny::HTML(paste0("<pre class=\"stv-code\"><code>", stv_escape_html(result@code), "</code></pre>"))
      )
    )
  )
}

#' Escape text for HTML
#'
#' Used for code blocks, where whitespace must not be reformatted.
#'
#' @param x A string.
#'
#' @return The escaped string.
#' @noRd
stv_escape_html <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  gsub(">", "&gt;", x, fixed = TRUE)
}
