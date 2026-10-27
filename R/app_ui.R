#' The application user interface
#'
#' @param request Internal parameter for `{shiny}`.
#' @param profile `NULL` or one of `stv_profiles()`. When `NULL`, the profile
#'   set by [run_app()] is used.
#'
#' @return A `bslib::page_navbar()`.
#' @noRd
app_ui <- function(request, profile = NULL) {
  profile <- if (is.null(profile)) stv_app_profile() else stv_resolve_profile(profile)
  public <- stv_is_public(profile)

  bslib::page_navbar(
    title = stv_navbar_title(),
    id = "main_nav",
    theme = stv_theme(),
    window_title = "stabilitest.viz",
    lang = "en",
    fillable = FALSE,
    header = golem_add_external_resources(),
    bslib::nav_panel("Analyse", value = "analyse", icon = bsicons::bs_icon("activity"), mod_analyse_ui("analyse", public)),
    bslib::nav_panel("Compare", value = "compare", icon = bsicons::bs_icon("layout-split"), mod_compare_ui("compare")),
    bslib::nav_panel("Learn", value = "learn", icon = bsicons::bs_icon("book"), mod_learn_ui()),
    bslib::nav_panel("About", value = "about", icon = bsicons::bs_icon("info-circle"), mod_about_ui()),
    bslib::nav_spacer(),
    if (!public) {
      bslib::nav_item(
        shiny::tags$div(
          class = "stv-nav-control",
          shiny::tags$span("Guided"),
          # The visible words sit around the switch; screen readers get the label
          bslib::input_switch("expert_mode", label = shiny::tags$span(class = "visually-hidden", "Expert mode"),
            value = FALSE),
          shiny::tags$span("Expert")
        )
      )
    },
    bslib::nav_item(
      shiny::tags$div(
        class = "stv-nav-control",
        bslib::input_dark_mode(id = "dark_mode")
      )
    )
  )
}

#' Brand lockup for the navbar
#'
#' Both variants are rendered and CSS shows the one that matches
#' `data-bs-theme`.
#'
#' @return A `shiny::tags` span.
#' @noRd
stv_navbar_title <- function() {
  shiny::tags$span(
    class = "stv-brand",
    shiny::tags$img(src = "www/lockup-light.svg", class = "stv-logo-light", alt = "stabilitest.viz"),
    shiny::tags$img(src = "www/lockup-dark.svg", class = "stv-logo-dark", alt = "stabilitest.viz")
  )
}

#' Add external resources to the app
#'
#' Registers `inst/app/www` and adds the favicon and apple-touch icon.
#'
#' @return A `shiny::tags` `head`.
#' @noRd
golem_add_external_resources <- function() {
  shiny::addResourcePath("www", app_sys("app", "www"))

  shiny::tags$head(
    shiny::tags$link(rel = "icon", type = "image/svg+xml", href = "www/favicon.svg"),
    shiny::tags$link(rel = "icon", type = "image/png", sizes = "32x32", href = "www/favicon-32.png"),
    shiny::tags$link(rel = "apple-touch-icon", sizes = "180x180", href = "www/favicon-180.png"),
    shiny::tags$meta(name = "description", content = "Interactive companion to the stabilitest R package.")
  )
}
