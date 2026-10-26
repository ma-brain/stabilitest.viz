#' Map module: assign columns to roles (sidebar step)
#'
#' @param id Module id.
#'
#' @return A `shiny::tagList()`.
#' @noRd
mod_map_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("inputs")),
    shiny::uiOutput(ns("formula"))
  )
}

#' Columns by kind
#'
#' @param data A data frame.
#'
#' @return A list of column-name vectors: `all`, `numeric`, `binary`, `group`.
#' @noRd
stv_column_kinds <- function(data) {
  is_binary <- vapply(data, \(x) {
    (is.numeric(x) || is.logical(x)) && length(unique(x[!is.na(x)])) > 0L && all(x[!is.na(x)] %in% c(0, 1))
  }, logical(1))
  n_unique <- vapply(data, \(x) length(unique(x[!is.na(x)])), integer(1))
  is_text <- vapply(data, \(x) is.character(x) || is.factor(x), logical(1))
  list(
    all = names(data),
    numeric = names(data)[vapply(data, is.numeric, logical(1))],
    binary = names(data)[is_binary],
    group = names(data)[is_text & n_unique >= 2L | is_binary | (n_unique >= 2L & n_unique <= 8L)]
  )
}

#' Pick a value that is still valid
#'
#' @param current The current input value.
#' @param choices Valid choices.
#' @param preferred A preferred default.
#'
#' @return One of `choices`, or `""` when there are none.
#' @noRd
stv_pick <- function(current, choices, preferred = NULL) {
  if (length(current) && current %in% choices) {
    return(current)
  }
  if (length(preferred) && preferred[[1]] %in% choices) {
    return(preferred[[1]])
  }
  if (length(choices)) choices[[1]] else ""
}

#' Map module server
#'
#' @param id Module id.
#' @param store Session store from `stv_store()`.
#' @param family A reactive list with `family` and `test`, from the Question step.
#'
#' @return A reactive list of mapping fields for the spec.
#' @noRd
mod_map_server <- function(id, store, family) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    role_col <- function(role) {
      s <- store$suggestion
      if (is.null(s)) character() else s$column[s$role == role]
    }
    is_paired <- function(fam) {
      stv_is_paired(list(family = fam$family, test = fam$test, paired = isTRUE(input$tost_paired)))
    }
    select <- function(inputId, label, choices, selected, multiple = FALSE) {
      shiny::selectInput(ns(inputId), label, choices = choices, selected = selected, multiple = multiple, selectize = multiple, width = "100%")
    }

    output$inputs <- shiny::renderUI({
      fam <- family()
      data <- store$data
      if (is.null(data) || is.null(fam$family)) {
        return(
          stv_empty_state(
            "Map your variables",
            "Load data and choose a question first, then pick the outcome and group here.",
            icon = "diagram-3", compact = TRUE
          )
        )
      }
      kinds <- stv_column_kinds(data)
      suggested <- shiny::isolate(list(
        outcome = role_col("Outcome"), group = role_col("Group"), time = role_col("Time"),
        event = role_col("Event"), covariates = role_col("Covariate"), id = role_col("Subject")
      ))
      keep <- function(x) shiny::isolate(x)

      if (fam$family %in% c("location", "proportion", "tost")) {
        binary <- fam$family == "proportion" || (fam$family == "tost" && fam$test %in% c("prop", "or"))
        outcome_choices <- if (binary) kinds$all else kinds$numeric
        paired <- is_paired(fam)
        layout <- input$layout %||% "wide"

        controls <- list()
        if (fam$family == "tost" && identical(fam$test, "mean")) {
          controls$paired <- shiny::checkboxInput(ns("tost_paired"), "Paired measurements", keep(input$tost_paired) %||% FALSE)
        }
        if (paired) {
          controls$layout <- shiny::radioButtons(
            ns("layout"), "Data layout",
            c("Two columns (before, after)" = "wide", "One column, one row per measurement" = "long"),
            selected = layout, width = "100%"
          )
        }
        if (paired && identical(layout, "wide")) {
          pair_choices <- kinds$numeric
          controls$outcome <- select("outcome", "First measurement", pair_choices, stv_pick(keep(input$outcome), pair_choices, pair_choices[1]))
          controls$outcome2 <- select("outcome2", "Second measurement", pair_choices, stv_pick(keep(input$outcome2), pair_choices, pair_choices[2]))
        } else {
          controls$outcome <- select(
            "outcome", if (binary) "Outcome (response)" else "Outcome", outcome_choices,
            stv_pick(keep(input$outcome), outcome_choices, suggested$outcome)
          )
          controls$success <- shiny::uiOutput(ns("success_ui"))
          controls$group <- select("group", "Group", kinds$group, stv_pick(keep(input$group), kinds$group, suggested$group))
          controls$levels <- shiny::uiOutput(ns("levels_ui"))
          if (paired) {
            controls$pair_id <- select("pair_id", "Pair identifier", kinds$all, stv_pick(keep(input$pair_id), kinds$all, suggested$id))
          }
        }
        if (fam$family == "proportion" && fam$test %in% c("chisq", "prop")) {
          controls$correct <- shiny::checkboxInput(ns("correct"), "Continuity correction", keep(input$correct) %||% TRUE)
        }
        if (fam$family == "tost") {
          controls$tost <- stv_tost_controls(ns, fam, shiny::isolate(shiny::reactiveValuesToList(input)))
        }
        return(controls)
      }

      # Models --------------------------------------------------------------
      controls <- list()
      if (fam$family == "cox") {
        controls$time <- select("time", "Time", kinds$numeric, stv_pick(keep(input$time), kinds$numeric, suggested$time))
        event_choices <- kinds$binary
        controls$event <- select("event", "Event", event_choices, stv_pick(keep(input$event), event_choices, suggested$event))
        controls$invert <- shiny::checkboxInput(
          ns("invert"), "Event is coded the other way (event = 1 - column, e.g. CNSR)",
          keep(input$invert) %||% any(grepl("1 - ", store$suggestion$note[store$suggestion$role == "Event"], fixed = TRUE))
        )
      } else {
        outcome_choices <- if (fam$family == "glm" && identical(fam$test, "binomial")) kinds$binary else kinds$numeric
        controls$outcome <- select("outcome", "Outcome", outcome_choices, stv_pick(keep(input$outcome), outcome_choices, suggested$outcome))
      }
      predictor_default <- unique(c(suggested$group, suggested$covariates))
      predictor_choices <- setdiff(kinds$all, c(input$outcome, input$time, input$event))
      controls$predictors <- select(
        "predictors", "Predictors (treatment and covariates)", predictor_choices,
        if (length(keep(input$predictors))) keep(input$predictors) else intersect(predictor_default, predictor_choices),
        multiple = TRUE
      )
      if (fam$family == "glm" && identical(fam$test, "poisson")) {
        controls$offset <- select("offset", "Exposure column (offset = log of it)", c("None" = "", kinds$numeric), keep(input$offset) %||% "")
      }
      controls$term <- shiny::uiOutput(ns("term_ui"))
      if (identical(store$mode, "expert")) {
        controls$weights <- if (fam$family %in% c("glm", "cox")) {
          select("obs_weights", "Observation weights (optional)", c("None" = "", kinds$numeric), keep(input$obs_weights) %||% "")
        }
        controls$reference <- shiny::uiOutput(ns("reference_ui"))
      }
      controls
    })

    output$success_ui <- shiny::renderUI({
      data <- store$data
      outcome <- input$outcome
      fam <- family()
      binary <- identical(fam$family, "proportion") || (identical(fam$family, "tost") && fam$test %in% c("prop", "or"))
      if (!binary || is.null(data) || !length(outcome) || !outcome %in% names(data)) {
        return(NULL)
      }
      kinds <- stv_column_kinds(data)
      if (outcome %in% kinds$binary) {
        return(NULL)
      }
      values <- sort(unique(as.character(stats::na.omit(data[[outcome]]))))
      select("success", "Value that counts as a response", values, stv_pick(shiny::isolate(input$success), values))
    })

    output$levels_ui <- shiny::renderUI({
      data <- store$data
      group <- input$group
      if (is.null(data) || !length(group) || !group %in% names(data)) {
        return(NULL)
      }
      values <- sort(unique(as.character(stats::na.omit(data[[group]]))))
      shiny::tagList(
        shiny::tags$div(
          class = "stv-pair",
          select("level1", "Group 1", values, stv_pick(shiny::isolate(input$level1), values, values[1])),
          select("level2", "Group 2", values, stv_pick(shiny::isolate(input$level2), setdiff(values, input$level1 %||% values[1]), values[2]))
        ),
        shiny::tags$p(class = "small text-body-secondary", "Effects are group 1 minus group 2.")
      )
    })

    model_fields <- shiny::reactive({
      fam <- family()
      list(
        outcome = input$outcome, predictors = input$predictors,
        offset = if (length(input$offset) && nzchar(input$offset)) input$offset,
        obs_weights = if (length(input$obs_weights) && nzchar(input$obs_weights)) input$obs_weights,
        time = input$time, event = input$event, event_invert = isTRUE(input$invert),
        reference = stv_reference_inputs(input, store$data, input$predictors)
      )
    })

    output$reference_ui <- shiny::renderUI({
      data <- store$data
      preds <- input$predictors
      if (is.null(data) || !length(preds)) {
        return(NULL)
      }
      text <- preds[vapply(data[preds], \(x) is.character(x) || is.factor(x), logical(1))]
      if (!length(text)) {
        return(NULL)
      }
      shiny::tags$details(
        class = "stv-details",
        shiny::tags$summary("Reference levels"),
        lapply(seq_along(text), \(i) {
          values <- sort(unique(as.character(stats::na.omit(data[[text[[i]]]]))))
          select(paste0("ref_", match(text[[i]], preds)), paste("Reference for", text[[i]]), values,
            stv_pick(shiny::isolate(input[[paste0("ref_", match(text[[i]], preds))]]), values, values[1]))
        })
      )
    })

    output$term_ui <- shiny::renderUI({
      fam <- family()
      data <- store$data
      if (is.null(data) || !length(input$predictors)) {
        return(NULL)
      }
      spec <- c(list(family = fam$family, test = fam$test), model_fields())
      spec <- stv_spec_finalize(spec, data)
      choices <- stv_term_choices(spec, data)
      if (!length(choices$all)) {
        return(NULL)
      }
      grouped <- list(
        "Coefficients" = choices$coefficients,
        "Whole term (joint test)" = setdiff(choices$terms, choices$coefficients)
      )
      grouped <- grouped[lengths(grouped) > 0L]
      default <- shiny::isolate(input$term)
      select("term", "Term to test", grouped, stv_pick(default, choices$all, choices$coefficients[1]))
    })

    output$formula <- shiny::renderUI({
      fam <- family()
      if (!fam$family %in% c("lm", "glm", "cox") || is.null(store$data)) {
        return(NULL)
      }
      spec <- c(list(family = fam$family, test = fam$test), model_fields())
      ok <- length(spec$predictors) && (fam$family == "cox" && length(spec$time) && length(spec$event) ||
        fam$family != "cox" && length(spec$outcome))
      if (!isTRUE(ok) || !nzchar(spec$outcome %||% "x")) {
        return(NULL)
      }
      shiny::tags$div(
        class = "mt-2",
        shiny::tags$div(class = "stv-section-title", "Model"),
        shiny::tags$code(class = "stv-formula", stv_formula_string(spec))
      )
    })

    shiny::reactive({
      fam <- family()
      if (is.null(fam$family)) {
        return(list())
      }
      if (fam$family %in% c("location", "proportion", "tost")) {
        paired <- is_paired(fam)
        layout <- input$layout %||% "wide"
        list(
          outcome = input$outcome,
          outcome2 = if (paired && identical(layout, "wide")) input$outcome2,
          group = if (!(paired && identical(layout, "wide"))) input$group,
          levels = if (!(paired && identical(layout, "wide")) && length(input$level1)) c(input$level1, input$level2),
          pair_id = if (paired && identical(layout, "long")) input$pair_id,
          paired_layout = if (paired) layout,
          paired = if (fam$family == "tost") isTRUE(input$tost_paired),
          success = if (length(input$success) && nzchar(input$success)) input$success,
          correct = if (fam$family == "proportion") isTRUE(input$correct %||% TRUE),
          tost_type = input$tost_type,
          margin = input$margin,
          delta_L = if (isTRUE(input$asym)) input$delta_L,
          delta_U = if (isTRUE(input$asym)) input$delta_U,
          higher_is_better = isTRUE(input$higher %||% TRUE)
        )
      } else {
        c(model_fields(), list(term = input$term))
      }
    })
  })
}

#' Controls specific to equivalence and non-inferiority
#'
#' @param ns Namespace function.
#' @param fam List with `family` and `test`.
#' @param input The module's input (read once, not reactively).
#'
#' @return A `shiny::tagList()`.
#' @noRd
stv_tost_controls <- function(ns, fam, input) {
  type <- input$tost_type %||% "equivalence"
  default_margin <- if (identical(fam$test, "or")) 1.25 else if (identical(fam$test, "prop")) 0.15 else 1
  shiny::tagList(
    shiny::selectInput(
      ns("tost_type"), "Claim",
      c("Equivalence" = "equivalence", "Non-inferiority" = "noninferiority"),
      selected = type, selectize = FALSE, width = "100%"
    ),
    shiny::numericInput(
      ns("margin"),
      switch(fam$test, mean = "Margin (difference in means)", prop = "Margin (risk difference)", or = "Margin (odds ratio, above 1)"),
      input$margin %||% default_margin, min = 0, step = if (identical(fam$test, "or")) 0.05 else 0.1, width = "100%"
    ),
    shiny::checkboxInput(ns("higher"), "Higher values are better", input$higher %||% TRUE)
  )
}

#' Reference levels chosen in the Map step
#'
#' @param input The module's input.
#' @param data The filtered data frame.
#' @param predictors Predictor column names.
#'
#' @return A named list, column to reference level.
#' @noRd
stv_reference_inputs <- function(input, data, predictors) {
  out <- list()
  for (i in seq_along(predictors)) {
    value <- input[[paste0("ref_", i)]]
    if (length(value) && nzchar(value)) {
      out[[predictors[[i]]]] <- value
    }
  }
  out
}
