#' Question module: what to test (sidebar step)
#'
#' @param id Module id.
#'
#' @return A `shiny::tagList()`.
#' @noRd
mod_question_ui <- function(id) {
  ns <- shiny::NS(id)
  families <- stv_families()

  shiny::tagList(
    shiny::radioButtons(
      ns("family"),
      label = NULL,
      choiceValues = families$id,
      choiceNames = lapply(seq_len(nrow(families)), \(i) {
        shiny::tags$span(
          class = "stv-choice",
          shiny::tags$span(class = "stv-choice-title", families$label[[i]]),
          shiny::tags$span(class = "stv-choice-text", families$question[[i]])
        )
      }),
      selected = character(0),
      width = "100%"
    ),
    shiny::uiOutput(ns("test_ui")),
    shiny::uiOutput(ns("note"))
  )
}

#' Question module server
#'
#' Picks the family from the suggested mapping when data are loaded.
#'
#' @param id Module id.
#' @param store Session store from `stv_store()`.
#'
#' @return A reactive list with `family` and `test`.
#' @noRd
mod_question_server <- function(id, store) {
  shiny::moduleServer(id, function(input, output, session) {
    # Test suggested for the loaded data; applied when the test list is drawn
    preferred_test <- shiny::reactiveVal(NULL)

    shiny::observeEvent(store$raw_data, {
      data <- shiny::isolate(store$data)
      suggestion <- shiny::isolate(store$suggestion)
      if (is.null(data)) {
        preferred_test(NULL)
        shiny::updateRadioButtons(session, "family", selected = character(0))
      } else {
        family <- stv_suggest_family(suggestion, data)
        test <- stv_suggest_test(suggestion, data, family)
        preferred_test(test)
        shiny::updateRadioButtons(session, "family", selected = family)
      }
    }, ignoreNULL = FALSE)

    # A test the user picks replaces the suggestion
    shiny::observeEvent(input$test, {
      if (!identical(input$test, preferred_test())) preferred_test(NULL)
    }, ignoreInit = TRUE)

    output$test_ui <- shiny::renderUI({
      family <- input$family
      tests <- if (length(family)) stv_tests(family)
      if (is.null(tests)) {
        return(NULL)
      }
      label <- switch(family, tost = "Endpoint", glm = "Outcome type", "Test")
      shiny::selectInput(
        session$ns("test"), label, choices = tests,
        selected = stv_pick(preferred_test(), tests, shiny::isolate(input$test)),
        selectize = FALSE, width = "100%"
      )
    })

    output$note <- shiny::renderUI({
      family <- input$family
      if (!length(family)) {
        return(NULL)
      }
      note <- if (identical(family, "cox") && !requireNamespace("survival", quietly = TRUE)) {
        "The survival package is not installed, so Cox models cannot run."
      } else if (identical(family, "tost")) {
        "Score cutoffs are not calibrated for equivalence or non-inferiority, so results are numeric only."
      }
      if (!is.null(note)) {
        shiny::tags$p(class = "small text-body-secondary mb-0", note)
      }
    })

    shiny::outputOptions(output, "test_ui", suspendWhenHidden = FALSE)

    shiny::reactive({
      family <- input$family
      if (!length(family)) {
        return(list())
      }
      tests <- stv_tests(family)
      test <- if (is.null(tests)) NULL else if (length(input$test) && input$test %in% tests) input$test else tests[[1]]
      list(family = family, test = test)
    })
  })
}
