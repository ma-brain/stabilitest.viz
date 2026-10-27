#' Data module: source, filters and checks (sidebar step)
#'
#' @param id Module id.
#' @param public Public profile: bundled examples only, no upload.
#'
#' @return A `shiny::tagList()`.
#' @noRd
mod_data_ui <- function(id, public = FALSE) {
  ns <- shiny::NS(id)
  examples <- stv_examples()
  by_group <- split(rlang::set_names(examples$id, examples$label), factor(examples$group, unique(examples$group)))
  example_select <- shiny::selectInput(
    ns("example"),
    label = if (public) "Example data" else NULL,
    choices = c(list("Choose an example..." = ""), by_group),
    selectize = FALSE,
    width = "100%"
  )

  shiny::tagList(
    if (public) {
      example_select
    } else {
      shiny::tagList(
        shiny::radioButtons(
          ns("source"),
          label = "Source",
          choices = c("Bundled example" = "example", "Upload a file" = "upload"),
          inline = TRUE
        ),
        shiny::conditionalPanel(
          condition = sprintf("input['%s'] == 'example'", ns("source")),
          example_select
        ),
        shiny::conditionalPanel(
          condition = sprintf("input['%s'] == 'upload'", ns("source")),
          shiny::fileInput(
            ns("file"),
            label = NULL,
            accept = paste0(".", names(stv_file_formats())),
            buttonLabel = "Browse...",
            placeholder = "CSV, Excel or SAS file",
            width = "100%"
          ),
          shiny::uiOutput(ns("sheet_ui"))
        )
      )
    },
    shiny::uiOutput(ns("status")),
    shiny::uiOutput(ns("filters")),
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
    profile <- shiny::isolate(store$profile)
    public <- stv_is_public(profile)
    max_mb <- stv_config("max_upload_mb", profile)
    read_error <- shiny::reactiveVal(NULL)

    # The public companion opens with an example already loaded.
    if (public) {
      shiny::updateSelectInput(session, "example", selected = stv_examples()$id[[1]])
    }

    # Examples ---------------------------------------------------------------
    shiny::observeEvent(input$example, {
      read_error(NULL)
      if (!nzchar(input$example)) {
        stv_store_set_data(store, NULL)
        return()
      }
      examples <- stv_examples()
      label <- examples$label[examples$id == input$example]
      stv_store_set_data(
        store, stv_load_example(input$example), label, input$example,
        source = list(id = input$example)
      )
    })

    # Upload -------------------------------------------------------------------
    if (!public) {
      iv <- shinyvalidate::InputValidator$new()
      iv$add_rule("file", function(file) {
        if (is.null(file)) {
          return(NULL)
        }
        stv_file_problem(file$name[[1]], file$size[[1]], max_mb)
      })
      iv$enable()

      sheets <- shiny::reactive({
        file <- input$file
        shiny::req(file)
        if (!identical(stv_file_format(file$name[[1]]), "excel") ||
          !is.null(stv_file_problem(file$name[[1]], file$size[[1]], max_mb))) {
          return(NULL)
        }
        tryCatch(stv_excel_sheets(file$datapath[[1]]), error = \(e) NULL)
      })
      chosen_sheet <- shiny::reactive({
        available <- sheets()
        if (length(input$sheet) && input$sheet %in% available) input$sheet else available[1]
      })

      output$sheet_ui <- shiny::renderUI({
        available <- sheets()
        if (length(available) > 1L) {
          shiny::selectInput(session$ns("sheet"), "Sheet", choices = available, selected = chosen_sheet(), width = "100%")
        }
      })

      shiny::observeEvent(list(input$file, chosen_sheet()), {
        file <- input$file
        shiny::req(file)
        read_error(NULL)
        if (!is.null(stv_file_problem(file$name[[1]], file$size[[1]], max_mb))) {
          return()
        }
        result <- tryCatch(
          stv_read_file(file$datapath[[1]], file$name[[1]], sheet = chosen_sheet()),
          stv_read_error = \(e) e
        )
        if (inherits(result, "error")) {
          read_error(conditionMessage(result))
          return()
        }
        stv_store_set_data(
          store, result$data, file$name[[1]], NULL, result$labels,
          source = list(file = file$name[[1]], format = result$format, sheet = if (identical(result$format, "excel")) chosen_sheet())
        )
      })
    }

    output$status <- shiny::renderUI({
      message <- read_error()
      if (!is.null(message)) {
        shiny::tags$div(class = "alert alert-warning py-2 small text-break", role = "alert", message)
      }
    })

    output$hint <- shiny::renderUI({
      text <- if (public) {
        "This public companion works with bundled examples only."
      } else if (identical(store$mode, "expert")) {
        paste(
          "Upload CSV, Excel or SAS (.xpt, .sas7bdat) files up to",
          sprintf("%s MB. SAS variable labels show as tooltips.", max_mb)
        )
      } else {
        "Pick an example or upload a file with one row per subject."
      }
      shiny::tags$p(class = "small text-body-secondary mt-2 mb-0", text)
    })

    # Filters ------------------------------------------------------------------
    output$filters <- shiny::renderUI({
      raw <- store$raw_data
      if (is.null(raw)) {
        return(NULL)
      }
      mode <- store$mode
      stv_filters_ui(session$ns, raw, shiny::isolate(store$adam), shiny::isolate(store$filters), expert = identical(mode, "expert"))
    })

    output$row_vals_ui <- shiny::renderUI({
      col <- input$row_col
      raw <- store$raw_data
      if (is.null(raw) || !length(col) || !nzchar(col) || !col %in% names(raw)) {
        return(NULL)
      }
      values <- utils::head(sort(unique(as.character(raw[[col]]))), 200L)
      shiny::selectizeInput(
        session$ns("row_vals"), "Keep values", choices = values,
        selected = shiny::isolate(store$filters$row_vals), multiple = TRUE, width = "100%"
      )
    })

    shiny::observeEvent(
      list(input$paramcd, input$avisit, input$flags, input$row_col, input$row_vals),
      {
        shiny::req(store$raw_data)
        stv_store_set_filters(store, list(
          paramcd = input$paramcd,
          avisit = input$avisit,
          flags = input$flags %||% character(),
          row_col = input$row_col,
          row_vals = input$row_vals
        ))
      },
      ignoreInit = TRUE
    )

    # Preview ------------------------------------------------------------------
    output$preview <- shiny::renderUI({
      data <- store$data
      if (is.null(data)) {
        return(
          bslib::card(
            stv_empty_state(
              title = "No data yet",
              text = if (public) {
                "Pick an example in the Data step to preview it here."
              } else {
                "Pick an example or upload a file in the Data step to preview it here."
              },
              icon = "table"
            )
          )
        )
      }
      stv_data_preview_card(
        data,
        label = store$data_label,
        id = store$data_id,
        labels = store$labels,
        adam = store$adam,
        checks = store$checks,
        suggestion = store$suggestion,
        n_total = nrow(store$raw_data)
      )
    })

    invisible(NULL)
  })
}

#' Filter inputs for the Data step
#'
#' Shows the detected structure, ADaM parameter, visit and flag filters when
#' applicable, and in Expert mode a free row filter.
#'
#' @param ns Namespace function of the module.
#' @param raw Unfiltered data.
#' @param adam Result of `stv_detect_adam()`.
#' @param filters Current filters, used as initial values.
#' @param expert Show the row filter.
#'
#' @return A `shiny::tagList()`.
#' @noRd
stv_filters_ui <- function(ns, raw, adam, filters, expert = FALSE) {
  v <- adam$vars
  controls <- list()

  if (adam$kind %in% c("BDS", "ADTTE")) {
    params <- unique(as.character(raw[[v$paramcd]]))
    if (!is.null(v$param)) {
      names_for <- vapply(params, \(p) {
        as.character(raw[[v$param]][match(p, as.character(raw[[v$paramcd]]))])
      }, character(1))
      params <- rlang::set_names(params, paste0(params, " - ", names_for))
    }
    controls$paramcd <- shiny::selectInput(
      ns("paramcd"), "Parameter", choices = params,
      selected = filters$paramcd, selectize = FALSE, width = "100%"
    )
    if (adam$kind == "BDS" && !is.null(v$avisit)) {
      controls$avisit <- shiny::selectInput(
        ns("avisit"), "Visit",
        choices = c("All visits" = "", stv_visit_levels(raw, adam)),
        selected = filters$avisit %||% "", selectize = FALSE, width = "100%"
      )
    }
  }

  if (length(v$flags)) {
    controls$flags <- shiny::checkboxGroupInput(
      ns("flags"), "Keep only flag = Y",
      choices = v$flags, selected = filters$flags, width = "100%"
    )
  }

  if (expert) {
    controls$row <- shiny::tags$details(
      class = "stv-details",
      shiny::tags$summary("Row filter (optional)"),
      shiny::selectInput(
        ns("row_col"), "Column",
        choices = c("None" = "", names(raw)),
        selected = filters$row_col %||% "", selectize = FALSE, width = "100%"
      ),
      shiny::uiOutput(ns("row_vals_ui"))
    )
  }

  shiny::tags$div(
    class = "stv-filters",
    shiny::tags$div(
      class = "mt-3 mb-2",
      shiny::tags$span(class = "stv-type", adam$label)
    ),
    controls
  )
}

#' Preview card for a data set
#'
#' @param data The filtered data frame.
#' @param label Display name.
#' @param id Example id, used to look up the description.
#' @param labels Named character vector of variable labels, or `NULL`.
#' @param adam Result of `stv_detect_adam()`.
#' @param checks Result of `stv_check_subjects()`.
#' @param suggestion Result of `stv_suggest_mapping()`.
#' @param n_total Rows before filtering.
#'
#' @return A `bslib::card()`.
#' @noRd
stv_data_preview_card <- function(data, label, id, labels = NULL, adam = NULL,
                                  checks = NULL, suggestion = NULL, n_total = nrow(data)) {
  columns <- stv_column_summary(data, labels)
  examples <- stv_examples()
  description <- if (is.null(id)) character() else examples$description[examples$id == id]

  stat <- function(value, name) {
    shiny::tags$div(
      class = "stv-stat",
      shiny::tags$span(class = "stv-stat-value", value),
      shiny::tags$span(class = "stv-stat-label", name)
    )
  }

  name_cell <- function(name, label) {
    if (nzchar(label)) {
      bslib::tooltip(shiny::tags$span(class = "stv-has-label", name), label)
    } else {
      name
    }
  }

  column_table <- stv_html_table(
    tibble::tibble(
      Column = unname(Map(name_cell, columns$name, columns$label)),
      Type = lapply(columns$type, stv_type_badge),
      Missing = columns$n_missing,
      `%` = columns$pct_missing
    ),
    caption = "Columns of the loaded data"
  )

  preview <- utils::head(data, 8)
  preview[] <- lapply(preview, \(x) if (is.factor(x)) as.character(x) else x)

  filtered <- nrow(data) != n_total
  rows_label <- if (filtered) sprintf("of %s rows", format(n_total, big.mark = ",")) else "rows"

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
        stat(format(nrow(data), big.mark = ","), rows_label),
        stat(ncol(data), "columns"),
        stat(sum(columns$n_missing), "missing cells"),
        if (!is.null(checks) && !is.na(checks$n_subjects)) {
          stat(format(checks$n_subjects, big.mark = ","), "subjects")
        }
      ),
      if (!is.null(checks)) shiny::tags$div(stv_check_badge(checks)),
      bslib::layout_columns(
        col_widths = c(5, 7),
        fill = FALSE,
        shiny::tags$div(
          shiny::tags$h2(class = "stv-section-title", "Columns"),
          column_table
        ),
        shiny::tags$div(
          shiny::tags$h2(class = "stv-section-title", "First rows"),
          stv_html_table(preview, caption = "First rows of the loaded data")
        )
      ),
      if (!is.null(suggestion)) stv_suggestion_block(suggestion, adam)
    )
  )
}

#' Badge for the one-row-per-subject check
#'
#' Neutral colours only: this is a data check, not a result label.
#'
#' @param checks Result of `stv_check_subjects()`.
#'
#' @return A `shiny::tags` span.
#' @noRd
stv_check_badge <- function(checks) {
  if (is.na(checks$ok)) {
    text <- "No subject column found, one row per subject not checked"
    icon <- "question-circle"
  } else if (checks$ok) {
    text <- "One row per subject"
    icon <- "check-circle"
  } else {
    text <- sprintf(
      "%s subjects have more than one row. Pick a parameter and visit, or filter rows.",
      format(checks$n_repeated, big.mark = ",")
    )
    icon <- "exclamation-circle"
  }
  shiny::tags$span(
    class = "stv-status",
    bsicons::bs_icon(icon, a11y = "deco"), " ", text
  )
}

#' Suggested mapping block
#'
#' @param suggestion Result of `stv_suggest_mapping()`.
#' @param adam Result of `stv_detect_adam()`.
#'
#' @return A `shiny::tags` div.
#' @noRd
stv_suggestion_block <- function(suggestion, adam) {
  source <- if (!is.na(adam$kind) && adam$kind != "ADSL") {
    paste("Preset for", adam$label)
  } else {
    "Guessed from column names and types"
  }

  shiny::tags$div(
    class = "mt-3",
    shiny::tags$h2(class = "stv-section-title", "Suggested mapping"),
    if (nrow(suggestion)) {
      stv_html_table(
        tibble::tibble(Role = suggestion$role, Column = suggestion$column, Note = suggestion$note),
        caption = "Suggested variable mapping"
      )
    } else {
      stv_empty_state(
        "No suggestion",
        "No columns could be matched to a role. You will choose them in the Map step.",
        icon = "diagram-3", compact = TRUE
      )
    },
    shiny::tags$p(
      class = "small text-body-secondary mt-2 mb-0",
      paste0(source, ". You will confirm or change this in the Map step.")
    )
  )
}
