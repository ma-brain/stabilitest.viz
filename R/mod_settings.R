#' Settings module (sidebar step)
#'
#' The advanced block is always rendered and shown only in Expert mode, so the
#' values exist in both modes and the validated-profile button works in both.
#'
#' @param id Module id.
#'
#' @return A `shiny::tagList()`.
#' @noRd
mod_settings_ui <- function(id) {
  ns <- shiny::NS(id)
  defaults <- stv_default_settings()

  shiny::tagList(
    shiny::numericInput(ns("alpha"), "Significance level (alpha)", defaults$alpha, min = 0.001, max = 0.5, step = 0.01, width = "100%"),
    shiny::numericInput(ns("n_boot"), "Bootstrap resamples", defaults$n_boot, min = 1, step = 100, width = "100%"),
    shiny::uiOutput(ns("profile")),
    shiny::conditionalPanel(
      condition = "input.expert_mode",
      shiny::tags$details(
        class = "stv-details", open = NA,
        shiny::tags$summary("Advanced"),
        shiny::sliderInput(ns("w_jackknife"), "Score weight: jackknife", 0, 1, defaults$weights[["jackknife"]], step = 0.05, width = "100%"),
        shiny::sliderInput(ns("w_fragility"), "Score weight: worst-case removal", 0, 1, defaults$weights[["fragility"]], step = 0.05, width = "100%"),
        shiny::uiOutput(ns("w_bootstrap")),
        shiny::numericInput(ns("max_removal"), "Maximum removal (% of observations)", 100 * defaults$max_removal_pct, min = 1, max = 50, step = 5, width = "100%"),
        shiny::numericInput(ns("threshold"), "Influence threshold, change in p (two-group tests)", defaults$influential_threshold, min = 0, max = 1, step = 0.01, width = "100%"),
        shiny::numericInput(ns("seed"), "Seed", defaults$seed, step = 1, width = "100%"),
        shiny::checkboxInput(ns("interpret"), "Write an interpretation (two-group tests)", defaults$interpret)
      )
    )
  )
}

#' Settings module server
#'
#' @param id Module id.
#' @param store Session store from `stv_store()`.
#' @param family A reactive list with `family` and `test`, from the Question step.
#'
#' @return A reactive settings list, see `stv_default_settings()`.
#' @noRd
mod_settings_server <- function(id, store, family) {
  shiny::moduleServer(id, function(input, output, session) {
    max_n_boot <- stv_config("max_n_boot", shiny::isolate(store$profile))
    defaults <- stv_default_settings()

    shiny::updateNumericInput(session, "n_boot", max = max_n_boot)

    weights <- shiny::reactive({
      jackknife <- input$w_jackknife %||% defaults$weights[["jackknife"]]
      fragility <- input$w_fragility %||% defaults$weights[["fragility"]]
      c(jackknife = jackknife, fragility = fragility, bootstrap = round(1 - jackknife - fragility, 4))
    })

    output$w_bootstrap <- shiny::renderUI({
      w <- weights()
      bad <- w[["bootstrap"]] < 0
      shiny::tags$p(
        class = paste("small", if (bad) "text-warning-emphasis" else "text-body-secondary"),
        if (bad) {
          "Jackknife and worst-case weights add up to more than 1."
        } else {
          sprintf("Bootstrap same-decision weight: %.2f (the three weights add up to 1).", w[["bootstrap"]])
        }
      )
    })

    settings <- shiny::reactive({
      list(
        alpha = input$alpha %||% defaults$alpha,
        n_boot = as.numeric(input$n_boot %||% defaults$n_boot),
        max_removal_pct = (input$max_removal %||% (100 * defaults$max_removal_pct)) / 100,
        influential_threshold = input$threshold %||% defaults$influential_threshold,
        weights = weights(),
        seed = input$seed %||% defaults$seed,
        interpret = input$interpret %||% defaults$interpret
      )
    })

    output$profile <- shiny::renderUI({
      fam <- family()
      if (!identical(fam$test, "fisher") || !identical(fam$family, "proportion")) {
        return(NULL)
      }
      status <- stv_profile_status(list(family = "proportion", test = "fisher", settings = settings()))
      shiny::tags$div(
        class = "stv-callout small mb-3",
        shiny::tags$p(class = "mb-2", status$message),
        if (!isTRUE(status$inside)) {
          shiny::actionButton(session$ns("use_fisher_profile"), "Use the validated Fisher settings", class = "btn-sm btn-outline-primary")
        }
      )
    })

    shiny::observeEvent(input$use_fisher_profile, {
      ref <- stv_fisher_profile_settings()
      shiny::updateNumericInput(session, "alpha", value = ref$alpha)
      shiny::updateNumericInput(session, "n_boot", value = ref$n_boot)
      shiny::updateSliderInput(session, "w_jackknife", value = ref$weights[["jackknife"]])
      shiny::updateSliderInput(session, "w_fragility", value = ref$weights[["fragility"]])
      shiny::updateNumericInput(session, "max_removal", value = 100 * ref$max_removal_pct)
    })

    settings
  })
}
