#' Access files in the current app
#'
#' @param ... Character vectors, specifying subdirectory and file(s) within
#'   the package `inst/` directory.
#'
#' @return A path inside the installed package.
#' @noRd
app_sys <- function(...) {
  system.file(..., package = "stabilitest.viz")
}

#' Valid configuration profiles
#'
#' @return A character vector of profile names, defined in
#'   `inst/golem-config.yml`.
#' @noRd
stv_profiles <- function() {
  c("default", "production", "public")
}

#' Resolve the configuration profile
#'
#' An explicit `profile` must be valid. When `NULL`, the `R_CONFIG_ACTIVE`
#' environment variable is used if it names a known profile, otherwise
#' `"default"`.
#'
#' @param profile `NULL` or one of `stv_profiles()`.
#'
#' @return A single profile name.
#' @noRd
stv_resolve_profile <- function(profile = NULL) {
  if (is.null(profile)) {
    active <- Sys.getenv("R_CONFIG_ACTIVE", unset = "")
    return(if (active %in% stv_profiles()) active else "default")
  }
  rlang::arg_match(profile, stv_profiles())
}

#' Read a value from the app configuration
#'
#' @param value Name of a key in `inst/golem-config.yml`.
#' @param profile `NULL` or one of `stv_profiles()`.
#'
#' @return The configured value.
#' @noRd
stv_config <- function(value, profile = NULL) {
  config::get(
    value = value,
    config = stv_resolve_profile(profile),
    file = app_sys("golem-config.yml")
  )
}

#' Is this the public companion profile?
#'
#' The public profile forces Guided mode and disables uploads.
#'
#' @param profile `NULL` or one of `stv_profiles()`.
#'
#' @return A single logical.
#' @noRd
stv_is_public <- function(profile = NULL) {
  identical(stv_config("app_profile", profile), "public")
}

#' Profile requested for the running app
#'
#' Reads the profile passed by [run_app()] through golem options.
#'
#' @return A single profile name.
#' @noRd
stv_app_profile <- function() {
  stv_resolve_profile(golem::get_golem_options("profile"))
}
