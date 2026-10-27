#' Results area
#'
#' The verdict (calibration banner, score row) above tabs that explore each
#' part of the result: removal path, influence, bootstrap, removed
#' observations, interpretation and code. Selecting observations in the
#' influence plot or the removed-observations table selects them in both.
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
    ns <- session$ns
    # Rows of the filtered data selected in a plot or table, shared by the tabs
    selected <- shiny::reactiveVal(integer())
    shiny::observeEvent(store$result, selected(integer()), ignoreNULL = FALSE)
    mode <- shiny::reactive(if (identical(store$theme, "dark")) "dark" else "light")

    output$results <- shiny::renderUI({
      result <- store$result
      if (is.null(result)) {
        return(mod_results_placeholder_ui())
      }
      shiny::tagList(stv_results_card(result), stv_explorer_ui(ns, result))
    })

    output$removal_plot <- plotly::renderPlotly({
      shiny::req(store$result)
      stv_plotly(stv_plot_removal(store$result, mode()), mode = mode())
    })

    output$influence_plot <- plotly::renderPlotly({
      shiny::req(store$result)
      w <- stv_plotly(stv_plot_influence(store$result, selected(), mode()), source = ns("influence"), mode = mode())
      w <- plotly::layout(w, dragmode = "select")
      for (event in c("plotly_click", "plotly_selected", "plotly_deselect")) w <- plotly::event_register(w, event)
      w
    })

    # Click or drag over points to select them; double-click clears
    influence_event <- \(event) stv_plotly_event(event, ns("influence"))
    shiny::observeEvent(influence_event("plotly_click"), {
      selected(as.integer(unlist(influence_event("plotly_click")$key)))
    })
    shiny::observeEvent(influence_event("plotly_selected"), {
      selected(as.integer(unlist(influence_event("plotly_selected")$key)))
    })
    shiny::observeEvent(influence_event("plotly_deselect"), selected(integer()))

    output$influence_rows <- reactable::renderReactable({
      result <- shiny::req(store$result)
      rows <- selected()
      shiny::validate(shiny::need(length(rows), "Click a point, or drag over several, to see their rows here."))
      stv_rows_table(result, store$data, rows)
    })

    output$bootstrap_plot <- plotly::renderPlotly({
      shiny::req(store$result)
      stv_plotly(stv_plot_bootstrap(store$result, mode()), mode = mode())
    })

    output$removed_table <- reactable::renderReactable({
      stv_removed_table(shiny::req(store$result), store$data)
    })
    shiny::observeEvent(reactable::getReactableState("removed_table", "selected"), {
      result <- store$result
      picked <- reactable::getReactableState("removed_table", "selected")
      steps <- result@removal[result@removal$k_removed > 0, , drop = FALSE]
      selected(as.integer(steps$row[picked]))
    }, ignoreNULL = FALSE, ignoreInit = TRUE)

    output$selection_note <- shiny::renderUI({
      rows <- selected()
      if (!length(rows)) {
        return(NULL)
      }
      shiny::tags$p(
        class = "small text-body-secondary mb-0",
        sprintf("%d selected (rows %s). They are ringed in the Influence plot.", length(rows), paste(utils::head(rows, 12), collapse = ", ")),
        if (length(rows) > 12) "..."
      )
    })

    invisible(NULL)
  })
}

#' Read a plotly event
#'
#' plotly warns, after every update, about events read before their plot has
#' been drawn, which is the case while the Influence tab is hidden. The event
#' is recorded in the list plotly checks (`plotlyShinyEventIDs`), as
#' `plotly::event_register()` does when the plot is drawn.
#'
#' @param event Event name, e.g. `"plotly_click"`.
#' @param source Plot source id.
#' @param session The Shiny session.
#'
#' @return The event data, or `NULL`.
#' @noRd
stv_plotly_event <- function(event, source, session = shiny::getDefaultReactiveDomain()) {
  ids <- session$userData$plotlyShinyEventIDs
  session$userData$plotlyShinyEventIDs <- unique(c(ids, paste(event, source, sep = "-")))
  plotly::event_data(event, source = source, session = session)
}

#' The tabs below the verdict
#'
#' @param ns Namespace function of the results module.
#' @param result An `stv_result`.
#'
#' @return A `bslib::navset_card_underline()`.
#' @noRd
stv_explorer_ui <- function(ns, result) {
  m <- result@metrics
  flips <- length(m$worstcase_fragility_k) && is.finite(m$worstcase_fragility_k)
  plot_height <- "380px"
  bslib::navset_card_underline(
    id = ns("tabs"),
    full_screen = TRUE,
    bslib::nav_panel(
      "Removal path",
      shiny::tags$p(
        class = "stv-tab-intro",
        "stabilitest removes one observation at a time, each time the one that moves the p-value furthest toward the other decision. ",
        if (flips) {
          sprintf("The decision changes after %s observations are removed.", stv_format_number(m$worstcase_fragility_k, 0))
        } else {
          "The decision did not change within the removal limit."
        },
        if (!is.null(result@extreme)) " The dashed line removes the most extreme values instead, for comparison; it is not part of the score."
      ),
      plotly::plotlyOutput(ns("removal_plot"), height = plot_height)
    ),
    bslib::nav_panel(
      "Influence",
      shiny::tags$p(
        class = "stv-tab-intro",
        "Each point is the p-value with one observation left out. The solid line is the original p-value, the dashed line alpha. ",
        "Points stabilitest flags as influential are marked. Click a point or drag over several to see their rows."
      ),
      plotly::plotlyOutput(ns("influence_plot"), height = plot_height),
      reactable::reactableOutput(ns("influence_rows"))
    ),
    bslib::nav_panel(
      "Bootstrap",
      shiny::tags$p(
        class = "stv-tab-intro",
        sprintf(
          "p-values of the %s bootstrap resamples. %s%% reach the same decision as the original analysis (the bootstrap same-decision rate). The solid line is the original p-value, the dashed line alpha.",
          format(sum(is.finite(result@bootstrap$p_value))), stv_format_number(m$bootstrap_same_decision_rate, 0)
        )
      ),
      plotly::plotlyOutput(ns("bootstrap_plot"), height = plot_height)
    ),
    bslib::nav_panel(
      "Removed observations",
      shiny::tags$p(
        class = "stv-tab-intro",
        "The observations the worst-case path removed, in order, with their rows from the data. ",
        "Select rows to ring them in the Influence plot."
      ),
      reactable::reactableOutput(ns("removed_table")),
      shiny::uiOutput(ns("selection_note"))
    ),
    bslib::nav_panel("Interpretation", stv_interpretation_ui(result)),
    bslib::nav_panel("Code", stv_code_block(result))
  )
}

#' Data rows of selected observations
#'
#' @param result An `stv_result`.
#' @param data The filtered data.
#' @param rows Row numbers in `data`.
#'
#' @return A `reactable::reactable()`.
#' @noRd
stv_rows_table <- function(result, data, rows) {
  jk <- result@jackknife[match(rows, result@jackknife$row), , drop = FALSE]
  df <- data.frame(
    Observation = jk$label, Row = rows,
    `p left out` = stv_tooltip_p(jk$p_value),
    Influential = ifelse(jk$influential %in% TRUE, "Flagged", ""),
    check.names = FALSE
  )
  df <- cbind(df, as.data.frame(data)[rows, , drop = FALSE])
  reactable::reactable(df, compact = TRUE, pagination = length(rows) > 10, defaultPageSize = 10,
    class = "stv-reactable", theme = stv_reactable_theme(), columns = list(Row = reactable::colDef(width = 70)))
}

#' Table theme that follows the page's light and dark colours
#'
#' @return A `reactable::reactableTheme()`.
#' @noRd
stv_reactable_theme <- function() {
  reactable::reactableTheme(
    color = "var(--bs-body-color)",
    backgroundColor = "transparent",
    borderColor = "var(--bs-border-color)",
    highlightColor = "var(--stv-mist)",
    cellPadding = "6px 8px",
    style = list(fontSize = "0.875rem", fontVariantNumeric = "tabular-nums"),
    headerStyle = list(color = "var(--stv-muted)", fontWeight = 600),
    rowSelectedStyle = list(backgroundColor = "var(--stv-mist)", boxShadow = "inset 3px 0 0 var(--bs-primary)"),
    inputStyle = list(backgroundColor = "transparent"),
    paginationStyle = list(color = "var(--stv-muted)")
  )
}

#' Table of the observations removed on the worst-case path
#'
#' @param result An `stv_result`.
#' @param data The filtered data.
#'
#' @return A `reactable::reactable()`.
#' @noRd
stv_removed_table <- function(result, data) {
  steps <- as.data.frame(result@removal[result@removal$k_removed > 0, , drop = FALSE])
  flip <- steps$k_removed[which(!steps$conclusion_match)[1]]
  df <- data.frame(
    Step = steps$k_removed, Observation = steps$label, Row = steps$row,
    `p after removal` = stv_tooltip_p(steps$p_value),
    Decision = ifelse(steps$conclusion_match %in% TRUE, "Same", "Changed"),
    check.names = FALSE
  )
  df <- cbind(df, as.data.frame(data)[steps$row, , drop = FALSE])
  reactable::reactable(
    df, compact = TRUE, selection = "multiple", onClick = "select", class = "stv-reactable",
    theme = stv_reactable_theme(),
    pagination = nrow(df) > 15, defaultPageSize = 15,
    rowClass = \(i) if (!is.na(flip) && df$Step[i] == flip) "stv-row-flip",
    columns = list(Step = reactable::colDef(width = 60), Row = reactable::colDef(width = 70))
  )
}

#' Interpretation text from stabilitest
#'
#' @param result An `stv_result`.
#'
#' @return Tags.
#' @noRd
stv_interpretation_ui <- function(result) {
  text <- result@interpretation
  if (is.null(text)) {
    note <- if (result@family %in% c("location", "proportion")) {
      "Turn on \u201cWrite an interpretation\u201d in the Settings step and run again to get stabilitest's text."
    } else {
      "stabilitest writes interpretation text for the two-group tests only."
    }
    return(shiny::tags$p(class = "stv-tab-intro", note))
  }
  parts <- c(
    overall = "Overall", jackknife = "Jackknife", worstcase = "Worst-case removal",
    bootstrap = "Bootstrap", recommendation = "Recommendation"
  )
  parts <- parts[names(parts) %in% names(text)]
  shiny::tagList(
    shiny::tags$p(class = "stv-tab-intro", "Text written by stabilitest for this result."),
    lapply(names(parts), \(nm) shiny::tags$div(
      class = "stv-interpretation",
      shiny::tags$h3(class = "stv-section-title", parts[[nm]]),
      shiny::tags$p(text[[nm]])
    ))
  )
}

#' Reproducible code with a copy button
#'
#' @param result An `stv_result`.
#'
#' @return Tags.
#' @noRd
stv_code_block <- function(result) {
  shiny::tags$div(
    class = "stv-code-wrap",
    shiny::tags$div(
      class = "d-flex align-items-center mb-2",
      shiny::tags$p(class = "stv-tab-intro mb-0", "Runs outside the app and gives the same result."),
      shiny::tags$button(
        type = "button", class = "btn btn-sm btn-outline-secondary ms-auto",
        onclick = "navigator.clipboard.writeText(this.closest('.stv-code-wrap').querySelector('code').innerText)",
        bsicons::bs_icon("clipboard", a11y = "deco"), " Copy"
      )
    ),
    shiny::HTML(paste0("<pre class=\"stv-code\"><code>", stv_escape_html(result@code), "</code></pre>"))
  )
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
      stv_score_formula(result)
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
