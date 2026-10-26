#' Run the stabilitest.viz app
#'
#' Launches the Shiny companion app for the stabilitest package.
#'
#' The profile controls limits and behaviour, as defined in
#' `inst/golem-config.yml`:
#'
#' * `"default"`: local use.
#' * `"production"`: self-hosted deployment with tighter limits.
#' * `"public"`: public companion. Guided mode only, uploads disabled and a
#'   bundled example preloaded.
#'
#' @param profile `NULL` or one of `"default"`, `"production"` and `"public"`.
#'   When `NULL`, the `R_CONFIG_ACTIVE` environment variable is used if it
#'   names one of these profiles, otherwise `"default"`.
#' @param onStart,options,enableBookmarking,uiPattern Passed to
#'   [shiny::shinyApp()].
#' @param ... Further arguments stored as golem options.
#'
#' @return A `shiny.appobj`. Printing it runs the app.
#' @export
#'
#' @examples
#' if (interactive()) {
#'   run_app()
#'   run_app(profile = "public")
#' }
run_app <- function(
  profile = NULL,
  onStart = NULL,
  options = list(),
  enableBookmarking = NULL,
  uiPattern = "/",
  ...
) {
  profile <- stv_resolve_profile(profile)

  golem::with_golem_options(
    app = shiny::shinyApp(
      ui = app_ui,
      server = app_server,
      onStart = stv_on_start(profile, onStart),
      options = options,
      enableBookmarking = enableBookmarking,
      uiPattern = uiPattern
    ),
    golem_opts = list(profile = profile, ...)
  )
}

#' Start-up hook of the app
#'
#' Applies the upload size limit of the profile to Shiny, starts the background
#' workers for analyses, then calls the user's own `onStart`.
#'
#' @param profile One of `stv_profiles()`.
#' @param onStart `NULL` or a function.
#'
#' @return A function.
#' @noRd
stv_on_start <- function(profile, onStart = NULL) {
  force(profile)
  force(onStart)
  function() {
    mb <- max(stv_config("max_upload_mb", profile), 1)
    old <- options(shiny.maxRequestSize = mb * 1024^2)
    shiny::onStop(function() options(old))
    stv_daemons_start(stv_config("workers", profile))
    shiny::onStop(stv_daemons_stop)
    if (!is.null(onStart)) onStart()
  }
}
