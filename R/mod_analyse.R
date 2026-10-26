#' Analyse page UI
#'
#' Sidebar stepper (Data, Question, Map, Settings, Run) next to the preview and
#' results area. Only the Data step works in M0.
#'
#' @param id Module id.
#' @param public Public profile: no upload.
#'
#' @return A `bslib::layout_sidebar()`.
#' @noRd
mod_analyse_ui <- function(id, public = FALSE) {
  ns <- shiny::NS(id)

  locked <- function(title, text, icon) {
    stv_empty_state(title, text, icon = icon, compact = TRUE)
  }

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
            active = TRUE,
            status = shiny::uiOutput(ns("data_status"), inline = TRUE, class = "stv-step-status")
          ),
          mod_data_ui(ns("data"), public)
        ),
        bslib::accordion_panel(
          value = "question",
          title = stv_step_title(2, "Question"),
          locked(
            "Choose what to test",
            "Pick the kind of comparison once your data is loaded: means, proportions, equivalence or a model.",
            "question-circle"
          )
        ),
        bslib::accordion_panel(
          value = "map",
          title = stv_step_title(3, "Map"),
          locked(
            "Map your variables",
            "Tell the app which column is the outcome and which is the group.",
            "diagram-3"
          )
        ),
        bslib::accordion_panel(
          value = "settings",
          title = stv_step_title(4, "Settings"),
          locked(
            "Review settings",
            "Significance level and number of bootstrap resamples will appear here.",
            "sliders"
          )
        ),
        bslib::accordion_panel(
          value = "run",
          title = stv_step_title(5, "Run"),
          locked(
            "Ready when you are",
            "The run button unlocks after the previous steps are complete.",
            "play-circle"
          ),
          shiny::tags$button(
            type = "button",
            class = "btn btn-primary w-100 mt-2",
            disabled = NA,
            "Run analysis"
          )
        )
      )
    ),
    mod_data_preview_ui(ns("data")),
    mod_results_placeholder_ui()
  )
}

#' Analyse page server
#'
#' @param id Module id.
#' @param store Session store from `stv_store()`.
#'
#' @return `NULL`, invisibly.
#' @noRd
mod_analyse_server <- function(id, store) {
  shiny::moduleServer(id, function(input, output, session) {
    mod_data_server("data", store)

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
