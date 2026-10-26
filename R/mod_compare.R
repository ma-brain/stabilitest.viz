#' Compare page UI
#'
#' @return A `shiny::tags` div.
#' @noRd
mod_compare_ui <- function() {
  shiny::tags$div(
    class = "stv-page",
    bslib::card(
      bslib::card_header("Compare analyses"),
      stv_empty_state(
        title = "Nothing to compare yet",
        text = paste(
          "Run two or more analyses in this session, for example Welch,",
          "Wilcoxon and Brunner-Munzel on the same data, and they will line up here."
        ),
        icon = "layout-split"
      )
    )
  )
}
