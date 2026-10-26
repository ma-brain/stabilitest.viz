#' Create the session store
#'
#' One `shiny::reactiveValues()` object shared by all modules. It holds the
#' loaded data, the analysis spec, the results history and the UI mode.
#'
#' @param profile `NULL` or one of `stv_profiles()`.
#'
#' @return A `shiny::reactiveValues()` object with fields `profile`, `mode`,
#'   `data`, `data_label`, `data_id`, `spec` and `history`.
#' @noRd
stv_store <- function(profile = NULL) {
  profile <- stv_resolve_profile(profile)

  shiny::reactiveValues(
    profile = profile,
    mode = "guided",
    data = NULL,
    data_label = NULL,
    data_id = NULL,
    spec = list(),
    history = list()
  )
}

#' Set the UI mode
#'
#' The public profile is always Guided, whatever is requested.
#'
#' @param store A store from `stv_store()`.
#' @param mode `"guided"` or `"expert"`.
#'
#' @return The mode that was applied, invisibly.
#' @noRd
stv_store_set_mode <- function(store, mode = c("guided", "expert")) {
  mode <- rlang::arg_match(mode)
  if (stv_is_public(shiny::isolate(store$profile))) {
    mode <- "guided"
  }
  store$mode <- mode
  invisible(mode)
}

#' Put a data set in the store
#'
#' Replacing the data clears the spec, because a spec refers to columns of the
#' old data. The results history is kept for the Compare page.
#'
#' @param store A store from `stv_store()`.
#' @param data A data frame, or `NULL` to clear.
#' @param label Display name.
#' @param id Identifier of the source, for example an example id.
#'
#' @return `store`, invisibly.
#' @noRd
stv_store_set_data <- function(store, data, label = NULL, id = NULL) {
  if (!is.null(data) && !is.data.frame(data)) {
    rlang::abort("`data` must be a data frame or NULL.")
  }
  store$data <- data
  store$data_label <- label
  store$data_id <- id
  store$spec <- list()
  invisible(store)
}

#' Append a result to the session history
#'
#' @param store A store from `stv_store()`.
#' @param result Any result object.
#'
#' @return `store`, invisibly.
#' @noRd
stv_store_add_result <- function(store, result) {
  store$history <- c(shiny::isolate(store$history), list(result))
  invisible(store)
}
