#' Analyse page UI
#'
#' Sidebar stepper (Data, Question, Map, Settings, Run) next to the preview and
#' results area.
#'
#' @param id Module id.
#' @param public Public profile: no upload.
#'
#' @return A `bslib::layout_sidebar()`.
#' @noRd
mod_analyse_ui <- function(id, public = FALSE) {
  ns <- shiny::NS(id)

  bslib::layout_sidebar(
    fillable = FALSE,
    sidebar = bslib::sidebar(
      id = ns("sidebar"),
      width = 360,
      open = list(desktop = "open", mobile = "always-above"),
      title = "Analysis",
      bslib::accordion(
        id = ns("steps"),
        class = "stv-stepper",
        open = "data",
        multiple = FALSE,
        bslib::accordion_panel(
          value = "data",
          title = stv_step_title(
            1, "Data",
            status = shiny::uiOutput(ns("data_status"), inline = TRUE, class = "stv-step-status")
          ),
          mod_data_ui(ns("data"), public)
        ),
        bslib::accordion_panel(
          value = "question",
          title = stv_step_title(2, "Question"),
          mod_question_ui(ns("question"))
        ),
        bslib::accordion_panel(
          value = "map",
          title = stv_step_title(3, "Map"),
          mod_map_ui(ns("map"))
        ),
        bslib::accordion_panel(
          value = "settings",
          title = stv_step_title(4, "Settings"),
          mod_settings_ui(ns("settings"))
        ),
        bslib::accordion_panel(
          value = "run",
          title = stv_step_title(5, "Run"),
          mod_run_ui(ns("run"))
        )
      )
    ),
    mod_data_preview_ui(ns("data")),
    mod_results_ui(ns("results"))
  )
}

#' Analyse page server
#'
#' Combines the Question, Map and Settings steps into the analysis spec in the
#' store.
#'
#' @param id Module id.
#' @param store Session store from `stv_store()`.
#' @param runner Passed to `mod_run_server()`.
#'
#' @return `NULL`, invisibly.
#' @noRd
mod_analyse_server <- function(id, store, runner = stv_run_async) {
  shiny::moduleServer(id, function(input, output, session) {
    mod_data_server("data", store)
    question <- mod_question_server("question", store)
    mapping <- mod_map_server("map", store, question)
    settings <- mod_settings_server("settings", store, question)
    mod_run_server("run", store, runner)
    mod_results_server("results", store)

    shiny::observe({
      data <- store$data
      fam <- question()
      if (is.null(data) || is.null(fam$family)) {
        store$spec <- list()
        return()
      }
      spec <- c(list(family = fam$family, test = fam$test), mapping(), list(settings = settings()))
      spec <- stv_spec_finalize(spec, data)
      spec$source <- c(store$source, list(filters = store$filters, adam = store$adam))
      store$spec <- spec
    })

    output$data_status <- shiny::renderUI({
      if (!is.null(store$data)) {
        bsicons::bs_icon("check-circle-fill", title = "Data loaded")
      }
    })

    invisible(NULL)
  })
}

#' Results area placeholder
#'
#' A ghost verdict strip and a neutral empty state. Nothing here is coloured
#' green or red: those colours are reserved for validated labels.
#'
#' @return A `bslib::card()`.
#' @noRd
mod_results_placeholder_ui <- function() {
  ghost <- function(label) {
    shiny::tags$div(
      class = "stv-ghost",
      shiny::tags$div(class = "stv-ghost-label", label),
      shiny::tags$div(class = "stv-ghost-value", "\u2014")
    )
  }

  bslib::card(
    bslib::card_header("Results"),
    bslib::card_body(
      bslib::layout_column_wrap(
        width = "11rem",
        fill = FALSE,
        ghost("Original p-value"),
        ghost("Composite score"),
        ghost("Removal fragility index"),
        ghost("Bootstrap same-decision rate")
      ),
      stv_empty_state(
        title = "No results yet",
        text = paste(
          "Load data and walk through the steps. Each number shown here",
          "will come straight from stabilitest."
        ),
        icon = "graph-up"
      )
    )
  )
}
