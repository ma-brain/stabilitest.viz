#' Empty state
#'
#' A calm placeholder for areas that have nothing to show yet.
#'
#' @param title Short heading.
#' @param text One or two sentences saying what to do or what comes next.
#' @param icon Name of a Bootstrap icon, see `bsicons::bs_icon()`.
#' @param compact Use the inline variant for narrow spaces such as the sidebar.
#'
#' @return A `shiny::tags` object.
#' @noRd
stv_empty_state <- function(title, text, icon = "inbox", compact = FALSE) {
  shiny::tags$div(
    class = paste("stv-empty", if (compact) "stv-empty-compact"),
    shiny::tags$span(
      class = "stv-empty-icon",
      bsicons::bs_icon(icon, a11y = "deco")
    ),
    shiny::tags$div(
      shiny::tags$div(class = "stv-empty-title", title),
      shiny::tags$p(text)
    )
  )
}

#' Stepper header with number badge
#'
#' @param n Step number.
#' @param title Step title.
#' @param active Highlight the badge.
#' @param status Optional extra content on the right, for example an icon.
#'
#' @return A `shiny::tags` object.
#' @noRd
stv_step_title <- function(n, title, active = FALSE, status = NULL) {
  shiny::tags$span(
    class = paste("d-flex align-items-center gap-2 w-100", if (active) "stv-step-active"),
    shiny::tags$span(class = "stv-step-num", n),
    shiny::tags$span(title),
    status
  )
}

#' Badge with a column type
#'
#' @param type Type label from `stv_column_type()`.
#'
#' @return A `shiny::tags` object.
#' @noRd
stv_type_badge <- function(type) {
  shiny::tags$span(class = "stv-type", type)
}

#' Format a value for a table cell
#'
#' @param x A scalar.
#'
#' @return A string, or a muted `NA` tag.
#' @noRd
stv_format_cell <- function(x) {
  if (is.na(x)) {
    return(shiny::tags$span(class = "text-body-tertiary", "NA"))
  }
  if (is.numeric(x)) {
    return(format(signif(x, 5), trim = TRUE, scientific = FALSE))
  }
  as.character(x)
}

#' Plain HTML table
#'
#' List columns hold ready-made cell content (tags or strings). Numeric columns
#' are right-aligned with tabular figures.
#'
#' @param data A data frame.
#' @param caption Accessible table caption (visually hidden).
#'
#' @return A `shiny::tags` object.
#' @noRd
stv_html_table <- function(data, caption) {
  numeric_cols <- vapply(data, is.numeric, logical(1))
  head <- lapply(seq_along(data), \(j) {
    shiny::tags$th(
      scope = "col",
      class = if (numeric_cols[[j]]) "num",
      names(data)[[j]]
    )
  })
  rows <- lapply(seq_len(nrow(data)), \(i) {
    cells <- lapply(seq_along(data), \(j) {
      col <- data[[j]]
      content <- if (is.list(col)) col[[i]] else stv_format_cell(col[[i]])
      shiny::tags$td(class = if (numeric_cols[[j]]) "num", content)
    })
    shiny::tags$tr(cells)
  })

  shiny::tags$div(
    class = "stv-table-wrap",
    shiny::tags$table(
      class = "table table-sm table-hover align-middle",
      shiny::tags$caption(class = "visually-hidden", caption),
      shiny::tags$thead(shiny::tags$tr(head)),
      shiny::tags$tbody(rows)
    )
  )
}

#' Explanatory card for the Learn page
#'
#' @param title Card title.
#' @param icon Bootstrap icon name.
#' @param question The plain-language question the metric answers.
#' @param ... Body content.
#'
#' @return A `bslib::card()`.
#' @noRd
stv_learn_card <- function(title, icon, question, ...) {
  bslib::card(
    class = "stv-learn",
    bslib::card_header(
      shiny::tags$span(class = "stv-learn-icon", bsicons::bs_icon(icon, a11y = "deco")),
      title
    ),
    bslib::card_body(
      shiny::tags$p(class = "stv-question", question),
      ...
    )
  )
}
