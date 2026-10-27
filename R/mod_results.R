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
  tone <- stv_label_tone(result)
  if (is.null(tone)) {
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
  shiny::tags$div(
    class = paste0("stv-banner stv-banner-", tone), role = "note",
    bsicons::bs_icon("patch-check", a11y = "deco"),
    shiny::tags$div(
      shiny::tags$div(class = "stv-banner-title", result@label),
      shiny::tags$div("Label returned by stabilitest under its validated calibration profile.")
    )
  )
}

#' Colour tone of a returned label
#'
#' @param result An `stv_result`.
#'
#' @return `"good"`, `"bad"` or `"neutral"` for a label stabilitest returned,
#'   `NULL` when there is none. Never derived from the score.
#' @noRd
stv_label_tone <- function(result) {
  if (identical(stv_calibration_state(result), "numeric_only")) {
    return(NULL)
  }
  if (grepl("not fragile|^robust", result@label, ignore.case = TRUE)) {
    "good"
  } else if (grepl("fragile", result@label, ignore.case = TRUE)) {
    "bad"
  } else {
    "neutral"
  }
}

#' Round a number for display
#'
#' @param x A number.
#' @param digits Decimal places.
#'
#' @return A string, `"n/a"` when missing.
#' @noRd
stv_format_number <- function(x, digits = 1) {
  if (length(x) && is.finite(x)) format(round(x, digits), nsmall = digits) else "n/a"
}

#' One box of the score row
#'
#' Every box has the same five rows (title, value, meter, caption, chip), so
#' boxes line up whatever their content.
#'
#' @param title Box title.
#' @param value Main value, a string.
#' @param unit Text shown small after the value.
#' @param meter A 0 to 100 value for the meter, or `NULL` for none. The meter
#'   has one colour whatever the value.
#' @param caption Caption text or tags.
#' @param chip Chip text, or `NULL`.
#' @param chip_class Extra chip class.
#' @param class Extra box class.
#'
#' @return A `shiny::tags` div.
#' @noRd
stv_score_box <- function(title, value, unit = NULL, meter = NULL, caption = NULL,
                          chip = NULL, chip_class = NULL, class = NULL) {
  meter_tag <- if (!is.null(meter) && length(meter) && is.finite(meter)) {
    pct <- max(0, min(100, meter))
    shiny::tags$div(
      class = "stv-meter", role = "img",
      `aria-label` = sprintf("%s of 100", stv_format_number(meter)),
      shiny::tags$span(style = sprintf("width: %.1f%%", pct))
    )
  } else {
    shiny::tags$div(class = "stv-meter-none")
  }
  shiny::tags$div(
    class = paste(c("stv-score", class), collapse = " "),
    shiny::tags$div(class = "stv-score-title", title),
    # Built as one string so no whitespace separates the value from "%"
    shiny::tags$div(
      class = "stv-score-value",
      shiny::HTML(paste0(
        stv_escape_html(value),
        if (!is.null(unit)) paste0("<small>", stv_escape_html(unit), "</small>")
      ))
    ),
    meter_tag,
    shiny::tags$div(class = "stv-score-caption", caption),
    if (is.null(chip)) {
      shiny::tags$span()
    } else {
      shiny::tags$span(class = paste(c("stv-chip", chip_class), collapse = " "), chip)
    }
  )
}

#' Weight chip text for a score component
#'
#' @param weights Named weights from stabilitest.
#' @param name Component name.
#'
#' @return A string, or `NULL` when stabilitest reported no weights.
#' @noRd
stv_weight_chip <- function(weights, name) {
  if (is.null(names(weights)) || !name %in% names(weights)) {
    return(NULL)
  }
  paste0("weight ", format(round(100 * weights[[name]])), "%")
}

#' Line showing how the composite score combines its components
#'
#' Prints the weights and components stabilitest returned next to its score.
#' Nothing is recomputed.
#'
#' @param result An `stv_result`.
#'
#' @return A `shiny::tags` div, or `NULL` when the weights are missing.
#' @noRd
stv_score_formula <- function(result) {
  w <- result@weights
  m <- result@metrics
  parts <- c(
    jackknife = m$jackknife_conclusion_stability,
    fragility = m$worstcase_fragility_component,
    bootstrap = m$bootstrap_same_decision_rate
  )
  if (!all(names(parts) %in% names(w)) || length(parts) != 3L) {
    return(NULL)
  }
  terms <- vapply(
    names(parts),
    \(nm) paste0(format(w[[nm]]), " \u00d7 ", format(round(parts[[nm]], 1))),
    character(1)
  )
  shiny::tags$div(
    class = "stv-score-formula",
    "Score ",
    shiny::tags$code(paste0(paste(terms, collapse = " + "), " = ", format(round(result@score, 1)))),
    " weights and components as returned by stabilitest"
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
  w <- result@weights
  decision <- if (identical(result@family, "tost")) {
    if (isTRUE(result@significant)) "Claim shown at this level" else "Claim not shown at this level"
  } else if (isTRUE(result@significant)) {
    sprintf("Significant at %s", format(result@alpha))
  } else {
    sprintf("Not significant at %s", format(result@alpha))
  }
  estimate <- if (length(result@estimate) && is.finite(result@estimate)) {
    paste0(
      "Estimate ", format(signif(result@estimate, 4)),
      if (all(is.finite(result@ci))) paste0(" (", paste(format(signif(result@ci, 4)), collapse = " to "), ")")
    )
  }
  k <- m$worstcase_fragility_k
  fragility_caption <- if (length(k) && is.finite(k)) {
    sprintf(
      "Decision flips after removing %s obs (%s%%)",
      stv_format_number(k, 0), stv_format_number(m$worstcase_fragility_pct)
    )
  } else {
    "Decision did not flip within the removal limit"
  }
  tone <- stv_label_tone(result)

  bslib::card(
    bslib::card_header(
      shiny::tags$span(result@method_label),
      shiny::tags$span(class = "stv-type ms-auto", paste("n =", result@n))
    ),
    bslib::card_body(
      stv_calibration_banner(result),
      shiny::tags$div(
        class = "stv-score-grid",
        stv_score_box(
          "Composite score", stv_format_number(result@score, 0),
          meter = result@score,
          caption = "Weighted mean of the three components, 0 to 100",
          chip = "result",
          class = c("stv-score-head", if (!is.null(tone)) paste0("stv-score-", tone))
        ),
        stv_score_box(
          "Jackknife stability", stv_format_number(m$jackknife_conclusion_stability, 0), "%",
          meter = m$jackknife_conclusion_stability,
          caption = "Leave-one-out runs keeping the decision",
          chip = stv_weight_chip(w, "jackknife"),
          chip_class = if (isTRUE(w[["jackknife"]] == 0)) "stv-chip-muted"
        ),
        stv_score_box(
          "Removal fragility", stv_format_number(m$worstcase_fragility_component, 0), " / 100",
          meter = m$worstcase_fragility_component,
          caption = fragility_caption,
          chip = stv_weight_chip(w, "fragility"),
          chip_class = if (isTRUE(w[["fragility"]] == 0)) "stv-chip-muted"
        ),
        stv_score_box(
          "Bootstrap same-decision rate", stv_format_number(m$bootstrap_same_decision_rate, 0), "%",
          meter = m$bootstrap_same_decision_rate,
          caption = "Resamples keeping the decision",
          chip = stv_weight_chip(w, "bootstrap"),
          chip_class = if (isTRUE(w[["bootstrap"]] == 0)) "stv-chip-muted"
        ),
        stv_score_box(
          "Original p-value", stv_format_p(result@p_value),
          caption = shiny::tagList(
            shiny::tags$div(class = "stv-score-decision", decision),
            estimate
          ),
          chip = "not in score", chip_class = "stv-chip-muted"
        )
      ),
      stv_score_formula(result),
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
