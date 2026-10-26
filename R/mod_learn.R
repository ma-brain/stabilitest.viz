#' Learn page UI
#'
#' Wording follows the stabilitest README.
#'
#' @return A `shiny::tags` div.
#' @noRd
mod_learn_ui <- function() {
  shiny::tags$div(
    class = "stv-page stv-learn",
    shiny::tags$div(
      class = "mb-4",
      shiny::tags$h1(class = "h3", "How stabilitest looks at a result"),
      shiny::tags$p(
        class = "text-body-secondary",
        "A p-value cannot tell you how easily a conclusion could be overturned.",
        "stabilitest asks that question with three complementary stress tests,",
        "then summarises them in one score."
      )
    ),
    bslib::layout_column_wrap(
      width = "30rem",
      heights_equal = "row",
      stv_learn_card(
        "Jackknife leave-one-out", "person-dash",
        "Which individual observations drive the result?",
        shiny::tags$p(
          "Each observation is left out in turn and the analysis is repeated.",
          "Observations whose removal moves the result most, or flips the",
          "decision, are flagged as influential."
        )
      ),
      stv_learn_card(
        "Worst-case removal", "scissors",
        "What is the smallest set of observations whose removal flips the conclusion?",
        shiny::tags$p(
          "A greedy search removes, one at a time, the observation that pushes the",
          "result furthest toward the opposite decision, in the spirit of the",
          "maximum influence perturbation of Broderick, Giordano & Meager (2023).",
          "The size of the smallest set found is the ",
          shiny::tags$strong("removal fragility index"), "."
        )
      ),
      stv_learn_card(
        "Bootstrap same-decision rate", "arrow-repeat",
        "How often do resamples drawn from the observed data preserve the original significance decision?",
        shiny::tags$p(
          "Following Goodman (1992), the data are resampled many times and the",
          "test is repeated. This is an empirical plug-in quantity, not the",
          "probability that a new trial will replicate the finding."
        )
      ),
      stv_learn_card(
        "Composite score", "speedometer2",
        "How can the three stress tests be read together?",
        shiny::tags$p(
          "A data-dependent composite score from 0 to 100 summarises the components.",
          "The numeric score and its component metrics are returned for every",
          "supported method."
        )
      )
    ),
    bslib::card(
      class = "mt-4",
      bslib::card_header(
        shiny::tags$span(class = "stv-learn-icon", bsicons::bs_icon("shield-check", a11y = "deco")),
        "A blank label is deliberate"
      ),
      bslib::card_body(
        shiny::tags$div(
          class = "stv-callout",
          shiny::tags$p(
            "For analysis types without validated cutoffs, stabilitest reports the",
            "numeric score and all stress-test details but leaves the categorical",
            "label blank. A blank label does not mean the analysis failed or that",
            "the score is missing. It means there is not enough calibration evidence",
            "to call that score robust or fragile."
          ),
          shiny::tags$p(
            class = "mb-0",
            "The current release prints a categorical label only for an applicable,",
            "statistically significant Fisher exact test. The About page lists the",
            "calibration status of every method."
          )
        )
      )
    )
  )
}
