#' Compare page UI
#'
#' Analyses run in this session, side by side. Every value is the one
#' stabilitest returned for that analysis.
#'
#' @param id Module id.
#'
#' @return A `shiny::tags` div.
#' @noRd
mod_compare_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tags$div(
    class = "stv-page",
    shiny::uiOutput(ns("page"))
  )
}

#' Compare page server
#'
#' @param id Module id.
#' @param store Session store from `stv_store()`.
#'
#' @return `NULL`, invisibly.
#' @noRd
mod_compare_server <- function(id, store) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    mode <- shiny::reactive(if (identical(store$theme, "dark")) "dark" else "light")
    has_history <- shiny::reactive(length(store$history) > 0L)

    output$page <- shiny::renderUI({
      if (!has_history()) {
        return(bslib::card(
          bslib::card_header("Compare analyses"),
          stv_empty_state(
            title = "Nothing to compare yet",
            text = paste(
              "Run two or more analyses in this session, for example Welch,",
              "Wilcoxon and Brunner-Munzel on the same data, and they will line up here."
            ),
            icon = "layout-split"
          )
        ))
      }
      shiny::tagList(
        bslib::card(
          bslib::card_header(
            shiny::tags$span("Analyses in this session"),
            shiny::tags$div(
              class = "ms-auto d-flex gap-2",
              shiny::actionButton(ns("remove"), "Remove selected", class = "btn-sm btn-outline-secondary",
                icon = bsicons::bs_icon("trash", a11y = "deco")),
              shiny::actionButton(ns("clear"), "Clear all", class = "btn-sm btn-outline-secondary")
            )
          ),
          shiny::tags$p(class = "stv-tab-intro", "Select the analyses to compare. The latest four are selected to start."),
          reactable::reactableOutput(ns("history"))
        ),
        shiny::uiOutput(ns("comparison"))
      )
    })

    output$history <- reactable::renderReactable({
      shiny::req(has_history())
      history <- store$history
      shiny::isolate(stv_history_table(history, utils::tail(seq_along(history), 4L)))
    })

    chosen <- shiny::reactive({
      history <- store$history
      picked <- reactable::getReactableState("history", "selected")
      history[picked[picked <= length(history)]]
    })

    shiny::observeEvent(input$remove, {
      ids <- vapply(chosen(), \(e) e$id, integer(1))
      stv_store_remove_results(store, ids)
    })
    shiny::observeEvent(input$clear, stv_store_remove_results(store, vapply(store$history, \(e) e$id, integer(1))))

    output$comparison <- shiny::renderUI({
      entries <- chosen()
      if (!length(entries)) {
        return(bslib::card(stv_empty_state(
          title = "No analyses selected", text = "Select one or more analyses above.", icon = "check2-square",
          compact = TRUE
        )))
      }
      bslib::card(
        bslib::card_header("Side by side"),
        if (!stv_same_data(entries)) {
          shiny::tags$div(
            class = "alert alert-info py-2 small",
            "These analyses were run on different data or different filters, so their numbers are not directly comparable."
          )
        },
        shiny::tags$p(
          class = "stv-tab-intro",
          "Each column is one analysis, with the numbers stabilitest returned for it. A label appears only where stabilitest returned one."
        ),
        reactable::reactableOutput(ns("table")),
        shiny::tags$h2(class = "stv-section-title mt-3", "Removal paths"),
        plotly::plotlyOutput(ns("paths"), height = "360px")
      )
    })

    output$table <- reactable::renderReactable({
      entries <- chosen()
      shiny::req(length(entries))
      df <- stv_compare_table(entries)
      reactable::reactable(
        df, compact = TRUE, sortable = FALSE, pagination = FALSE, class = "stv-reactable",
        theme = stv_reactable_theme(),
        columns = list(Measure = reactable::colDef(minWidth = 190, style = list(fontWeight = 600))),
        defaultColDef = reactable::colDef(minWidth = 160)
      )
    })

    output$paths <- plotly::renderPlotly({
      entries <- chosen()
      shiny::req(length(entries))
      stv_plotly(stv_plot_compare_removal(entries, mode()), mode = mode())
    })

    invisible(NULL)
  })
}

#' Table of the session history
#'
#' @param history History entries.
#' @param selected Positions to select to start.
#'
#' @return A `reactable::reactable()`.
#' @noRd
stv_history_table <- function(history, selected = NULL) {
  df <- data.frame(
    `#` = vapply(history, \(e) e$id, integer(1)),
    Time = vapply(history, \(e) format(e$time, "%H:%M:%S"), character(1)),
    Data = vapply(history, \(e) e$data_label %||% "Unnamed data", character(1)),
    Method = vapply(history, \(e) e$result@method_label, character(1)),
    n = vapply(history, \(e) as.integer(e$result@n), integer(1)),
    p = vapply(history, \(e) stv_format_p(e$result@p_value), character(1)),
    Score = vapply(history, \(e) stv_format_number(e$result@score, 0), character(1)),
    Label = vapply(history, \(e) if (is.na(e$result@label)) "None" else e$result@label, character(1)),
    check.names = FALSE
  )
  reactable::reactable(
    df, compact = TRUE, selection = "multiple", onClick = "select", defaultSelected = selected,
    class = "stv-reactable", theme = stv_reactable_theme(), pagination = nrow(df) > 10,
    columns = list(`#` = reactable::colDef(width = 50), Time = reactable::colDef(width = 90),
      n = reactable::colDef(width = 70), Score = reactable::colDef(width = 80))
  )
}
