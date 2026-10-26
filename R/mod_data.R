#' Data module: bundled example picker (sidebar step)
#'
#' @param id Module id.
#'
#' @return A `shiny::tagList()`.
#' @noRd
mod_data_ui <- function(id) {
  ns <- shiny::NS(id)
  examples <- stv_examples()

  shiny::tagList(
    shiny::selectInput(
      ns("example"),
      label = "Example data",
      choices = c("Choose an example..." = "", rlang::set_names(examples$id, examples$label)),
      selectize = FALSE,
      width = "100%"
    ),
    shiny::uiOutput(ns("hint"))
  )
}

#' Data module: preview (main area)
#'
#' @param id Module id, the same as for `mod_data_ui()`.
#'
#' @return A `shiny::uiOutput()`.
#' @noRd
mod_data_preview_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("preview"))
}

#' Data module server
#'
#' @param id Module id.
#' @param store Session store from `stv_store()`.
#'
#' @return `NULL`, invisibly. State goes through `store`.
#' @noRd
mod_data_server <- function(id, store) {
  shiny::moduleServer(id, function(input, output, session) {
    # The public companion opens with an example already loaded.
    if (stv_is_public(shiny::isolate(store$profile))) {
      shiny::updateSelectInput(session, "example", selected = stv_examples()$id[[1]])
    }

    shiny::observeEvent(input$example, {
      if (!nzchar(input$example)) {
        stv_store_set_data(store, NULL)
        return()
      }
      examples <- stv_examples()
      label <- examples$label[examples$id == input$example]
      stv_store_set_data(store, stv_load_example(input$example), label, input$example)
    })

    output$hint <- shiny::renderUI({
      public <- stv_is_public(store$profile)
      text <- if (identical(store$mode, "expert")) {
        "Bundled data from stabilitest. Upload (CSV, XLSX, SAS) arrives in M1."
      } else {
        "Start with an example to see how the app works. You will be able to upload your own file soon."
      }
      if (public) {
        text <- "This public companion works with bundled examples only."
      }
      shiny::tags$p(class = "small text-body-secondary mb-0", text)
    })

    output$preview <- shiny::renderUI({
      data <- store$data
      if (is.null(data)) {
        return(
          bslib::card(
            stv_empty_state(
              title = "No data yet",
              text = "Pick an example in the Data step to preview it here.",
              icon = "table"
            )
          )
        )
      }
      stv_data_preview_card(data, store$data_label, store$data_id)
    })

    invisible(NULL)
  })
}

#' Preview card for a data set
#'
#' @param data A data frame.
#' @param label Display name.
#' @param id Example id, used to look up the description.
#'
#' @return A `bslib::card()`.
#' @noRd
stv_data_preview_card <- function(data, label, id) {
  columns <- stv_column_summary(data)
  examples <- stv_examples()
  description <- examples$description[examples$id == id]

  stat <- function(value, name) {
    shiny::tags$div(
      class = "stv-stat",
      shiny::tags$span(class = "stv-stat-value", value),
      shiny::tags$span(class = "stv-stat-label", name)
    )
  }

  column_table <- stv_html_table(
    tibble::tibble(
      Column = columns$name,
      Type = lapply(columns$type, stv_type_badge),
      Missing = columns$n_missing
    ),
    caption = "Columns of the loaded data"
  )

  preview <- utils::head(data, 8)
  preview[] <- lapply(preview, \(x) if (is.factor(x)) as.character(x) else x)

  bslib::card(
    full_screen = TRUE,
    bslib::card_header(
      shiny::tags$span(label),
      shiny::tags$span(
        class = "stv-type ms-auto",
        sprintf("%d x %d", nrow(data), ncol(data))
      )
    ),
    bslib::card_body(
      if (length(description)) shiny::tags$p(class = "text-body-secondary", description),
      bslib::layout_column_wrap(
        width = "6rem",
        fill = FALSE,
        stat(format(nrow(data), big.mark = ","), "rows"),
        stat(ncol(data), "columns"),
        stat(sum(columns$n_missing), "missing cells")
      ),
      bslib::layout_columns(
        col_widths = c(4, 8),
        fill = FALSE,
        shiny::tags$div(
          shiny::tags$h3(class = "stv-section-title", "Columns"),
          column_table
        ),
        shiny::tags$div(
          shiny::tags$h3(class = "stv-section-title", "First rows"),
          stv_html_table(preview, caption = "First rows of the loaded data")
        )
      )
    )
  )
}
