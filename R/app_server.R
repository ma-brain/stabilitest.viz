#' The application server-side logic
#'
#' @param input,output,session Internal parameters for `{shiny}`.
#' @param profile `NULL` or one of `stv_profiles()`. When `NULL`, the profile
#'   set by [run_app()] is used.
#'
#' @return `NULL`, invisibly.
#' @noRd
app_server <- function(input, output, session, profile = NULL) {
  profile <- if (is.null(profile)) stv_app_profile() else stv_resolve_profile(profile)

  store <- stv_store(profile)

  shiny::observeEvent(input$expert_mode, {
    stv_store_set_mode(store, if (isTRUE(input$expert_mode)) "expert" else "guided")
  })

  # Plots follow the light/dark switch
  shiny::observeEvent(input$dark_mode, store$theme <- input$dark_mode)

  mod_analyse_server("analyse", store)
  mod_compare_server("compare", store)

  invisible(NULL)
}
