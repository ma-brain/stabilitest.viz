#' Run module: run button, problems and cancel (sidebar step)
#'
#' @param id Module id.
#'
#' @return A `shiny::tagList()`.
#' @noRd
mod_run_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("problems")),
    shiny::uiOutput(ns("estimate")),
    shiny::div(
      class = "d-flex gap-2 mt-2",
      bslib::input_task_button(
        ns("run"), "Run analysis",
        label_busy = "Running...", class = "btn-primary flex-grow-1"
      ),
      shiny::uiOutput(ns("cancel_ui"), inline = TRUE)
    ),
    shiny::uiOutput(ns("status"))
  )
}

#' Run module server
#'
#' Validates the spec, then runs stabilitest in a background process through an
#' `ExtendedTask` so the app stays responsive. A cancel button stops the
#' background task.
#'
#' @param id Module id.
#' @param store Session store from `stv_store()`.
#' @param runner Function `(plan, data)` returning a list with `task` (a
#'   promise-like that resolves to the raw result), `row_map` and `prepared`.
#'   Defaults to `stv_run_async()`; tests substitute a stub.
#'
#' @return `NULL`, invisibly.
#' @noRd
mod_run_server <- function(id, store, runner = stv_run_async) {
  shiny::moduleServer(id, function(input, output, session) {
    max_n_boot <- stv_config("max_n_boot", shiny::isolate(store$profile))
    pending <- new.env()
    running <- shiny::reactiveVal(FALSE)
    message <- shiny::reactiveVal(NULL)
    attempted <- shiny::reactiveVal(FALSE)

    problems <- shiny::reactive({
      spec <- store$spec
      if (!length(spec)) {
        return("Complete the previous steps first.")
      }
      stv_validate_spec(spec, store$data, max_n_boot = max_n_boot, checks = store$checks)
    })

    output$problems <- shiny::renderUI({
      p <- problems()
      if (!length(p) || !attempted()) {
        return(NULL)
      }
      shiny::tags$div(
        class = "alert alert-warning py-2 small", role = "alert",
        shiny::tags$ul(class = "mb-0 ps-3", lapply(p, shiny::tags$li))
      )
    })

    output$estimate <- shiny::renderUI({
      spec <- store$spec
      if (!length(spec) || length(problems())) {
        return(NULL)
      }
      seconds <- stv_estimate_runtime(spec, nrow(store$data))
      if (seconds >= 5) {
        shiny::tags$p(
          class = "small text-body-secondary mb-0",
          sprintf("This may take about %s. The app stays usable while it runs.", stv_format_seconds(seconds))
        )
      }
    })

    task <- shiny::ExtendedTask$new(function(plan, data) {
      run <- runner(plan, data)
      pending$run <- run
      run$task
    })
    bslib::bind_task_button(task, "run")

    shiny::observeEvent(input$run, {
      attempted(TRUE)
      message(NULL)
      if (length(problems())) {
        bslib::update_task_button("run", state = "ready")
        return()
      }
      spec <- store$spec
      pending$spec <- spec
      pending$plan <- stv_plan(spec)
      pending$started <- Sys.time()
      pending$cancelled <- FALSE
      running(TRUE)
      task$invoke(pending$plan, store$data)
    })

    shiny::observeEvent(input$cancel, {
      pending$cancelled <- TRUE
      if (!is.null(pending$run$task) && inherits(pending$run$task, "mirai")) {
        mirai::stop_mirai(pending$run$task)
      }
    })

    shiny::observeEvent(task$status(), {
      status <- task$status()
      if (identical(status, "running")) {
        return()
      }
      running(FALSE)
      if (!identical(status, "success") && !identical(status, "error")) {
        return()
      }
      raw <- tryCatch(task$result(), error = \(e) e)
      if (isTRUE(pending$cancelled)) {
        message(list(type = "info", text = "Run cancelled."))
        return()
      }
      if (inherits(raw, "error")) {
        message(list(type = "error", text = paste("stabilitest could not run this analysis:", stv_error_text(raw))))
        return()
      }
      result <- tryCatch(
        stv_as_result(
          raw, pending$spec, pending$plan, pending$run$prepared, pending$run$row_map,
          elapsed = as.numeric(difftime(Sys.time(), pending$started, units = "secs"))
        ),
        error = \(e) e
      )
      if (inherits(result, "error")) {
        message(list(type = "error", text = paste("The result could not be read:", conditionMessage(result))))
        return()
      }
      store$result <- result
      stv_store_add_result(store, result)
    })

    output$cancel_ui <- shiny::renderUI({
      if (running()) {
        shiny::actionButton(session$ns("cancel"), "Cancel", class = "btn-outline-secondary")
      }
    })

    output$status <- shiny::renderUI({
      m <- message()
      if (is.null(m)) {
        return(NULL)
      }
      shiny::tags$div(
        class = paste("alert py-2 small mt-2 text-break", if (identical(m$type, "error")) "alert-warning" else "alert-secondary"),
        role = "alert", m$text
      )
    })

    invisible(NULL)
  })
}

#' Format seconds for people
#'
#' @param seconds A number.
#'
#' @return A string such as `"20 seconds"` or `"3 minutes"`.
#' @noRd
stv_format_seconds <- function(seconds) {
  if (seconds < 90) {
    sprintf("%d seconds", as.integer(5 * ceiling(seconds / 5)))
  } else {
    sprintf("%d minutes", as.integer(round(seconds / 60)))
  }
}

#' Error text without R's "Error in <call> :" prefix
#'
#' @param e A condition.
#'
#' @return A string.
#' @noRd
stv_error_text <- function(e) {
  trimws(sub("(?s)^Error(?: in .*? )?\\s*:\\s*", "", conditionMessage(e), perl = TRUE))
}
