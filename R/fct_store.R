#' Create the session store
#'
#' One `shiny::reactiveValues()` object shared by all modules. It holds the
#' loaded data, the analysis spec, the results history and the UI mode.
#'
#' `raw_data` is the data as loaded and `data` is what analyses use, after
#' filters. `adam`, `filters`, `checks` and `suggestion` are derived from them
#' by `stv_store_set_data()` and `stv_store_set_filters()`. `theme` follows the
#' light/dark switch, for plots.
#'
#' @param profile `NULL` or one of `stv_profiles()`.
#'
#' @return A `shiny::reactiveValues()` object.
#' @noRd
stv_store <- function(profile = NULL) {
  profile <- stv_resolve_profile(profile)

  shiny::reactiveValues(
    profile = profile,
    mode = "guided",
    theme = "light",
    raw_data = NULL,
    data = NULL,
    data_label = NULL,
    data_id = NULL,
    labels = NULL,
    source = NULL,
    adam = NULL,
    filters = NULL,
    checks = NULL,
    suggestion = NULL,
    spec = list(),
    result = NULL,
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
#' ADaM structure is detected and default filters are applied. Replacing the
#' data clears the spec, because a spec refers to columns of the old data. The
#' results history is kept for the Compare page.
#'
#' @param store A store from `stv_store()`.
#' @param data A data frame, or `NULL` to clear.
#' @param label Display name.
#' @param id Identifier of the source, for example an example id.
#' @param labels Named character vector of variable labels, or `NULL`.
#' @param source A list describing where the data came from, used for the
#'   reproducible code: `id` for an example, or `file`, `format` and `sheet`.
#'
#' @return `store`, invisibly.
#' @noRd
stv_store_set_data <- function(store, data, label = NULL, id = NULL, labels = NULL, source = NULL) {
  if (!is.null(data) && !is.data.frame(data)) {
    rlang::abort("`data` must be a data frame or NULL.")
  }
  store$spec <- list()
  store$data_label <- label
  store$data_id <- id
  store$labels <- labels
  store$source <- source
  store$result <- NULL
  store$raw_data <- data

  if (is.null(data)) {
    store$data <- NULL
    store$adam <- NULL
    store$filters <- NULL
    store$checks <- NULL
    store$suggestion <- NULL
    return(invisible(store))
  }

  adam <- stv_detect_adam(data)
  store$adam <- adam
  stv_store_set_filters(store, stv_default_filters(data, adam))
}

#' Set the filters and refresh the filtered data
#'
#' @param store A store from `stv_store()`.
#' @param filters A filter list, see `stv_apply_filters()`.
#'
#' @return `store`, invisibly.
#' @noRd
stv_store_set_filters <- function(store, filters) {
  raw <- shiny::isolate(store$raw_data)
  adam <- shiny::isolate(store$adam)
  if (is.null(raw)) {
    return(invisible(store))
  }

  data <- stv_apply_filters(raw, adam, filters)
  store$filters <- filters
  store$data <- data
  store$checks <- stv_check_subjects(data, adam)
  store$suggestion <- stv_suggest_mapping(data, adam)
  store$spec <- list()
  store$result <- NULL
  invisible(store)
}

#' Append a result to the session history
#'
#' Each entry records the result with the data it was run on, for the Compare
#' page: `id` (numbered from 1 in the session), `result`, `data_label`,
#' `data_id`, `n_rows` (rows after filters) and `time`.
#'
#' @param store A store from `stv_store()`.
#' @param result An `stv_result`.
#'
#' @return `store`, invisibly.
#' @noRd
stv_store_add_result <- function(store, result) {
  history <- shiny::isolate(store$history)
  ids <- vapply(history, \(e) e$id, integer(1))
  entry <- list(
    id = if (length(ids)) max(ids) + 1L else 1L,
    result = result,
    data_label = shiny::isolate(store$data_label),
    data_id = shiny::isolate(store$data_id),
    n_rows = nrow(shiny::isolate(store$data)) %||% NA_integer_,
    time = Sys.time()
  )
  store$history <- c(history, list(entry))
  invisible(store)
}

#' Remove entries from the session history
#'
#' @param store A store from `stv_store()`.
#' @param ids Entry ids to remove.
#'
#' @return `store`, invisibly.
#' @noRd
stv_store_remove_results <- function(store, ids) {
  history <- shiny::isolate(store$history)
  store$history <- Filter(\(e) !e$id %in% ids, history)
  invisible(store)
}
