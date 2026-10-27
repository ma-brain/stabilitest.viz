#' Plot colours for a theme
#'
#' One brand colour for data, the accent for marked points. Green and red are
#' never used: they are reserved for labels stabilitest returns.
#'
#' @param mode `"light"` or `"dark"`.
#'
#' @return A named list of colours.
#' @noRd
stv_plot_colours <- function(mode = "light") {
  pal <- stv_palette(if (identical(mode, "dark")) "dark" else "light")
  list(
    bg = pal$surface, fg = pal$fg, muted = pal$muted, grid = pal$border,
    main = pal$primary, second = pal$accent, marked = pal$accent
  )
}

#' Shared ggplot2 theme elements
#'
#' @param col Colours from `stv_plot_colours()`.
#'
#' @return A ggplot2 theme.
#' @noRd
stv_plot_theme <- function(col) {
  ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      panel.grid.major = ggplot2::element_line(colour = col$grid, linewidth = 0.3),
      panel.grid.minor = ggplot2::element_blank(),
      axis.title = ggplot2::element_text(colour = col$muted, size = 11),
      axis.text = ggplot2::element_text(colour = col$muted),
      legend.position = "top",
      legend.title = ggplot2::element_blank(),
      plot.background = ggplot2::element_rect(fill = col$bg, colour = NA),
      panel.background = ggplot2::element_rect(fill = col$bg, colour = NA),
      plot.caption = ggplot2::element_text(colour = col$muted, size = 10, hjust = 0),
      text = ggplot2::element_text(colour = col$fg)
    )
}

#' Format a p-value for a tooltip
#'
#' @param p Numbers.
#'
#' @return Strings.
#' @noRd
stv_tooltip_p <- function(p) {
  ifelse(is.na(p), "NA", ifelse(p < 1e-4, format(p, digits = 2, scientific = TRUE), format(signif(p, 3))))
}

#' The p-value axis
#'
#' p-values are shown on a log scale so both very small values and the
#' significance level are readable.
#'
#' @param name Axis title.
#'
#' @return A ggplot2 scale.
#' @noRd
stv_p_axis <- function(name) {
  ggplot2::scale_y_log10(name = name, labels = \(x) format(x, scientific = FALSE, drop0trailing = TRUE, trim = TRUE))
}

#' Removal path plot
#'
#' p-value after removing k observations, for the worst-case path and (two-group
#' tests) the extreme-value path, with the significance level and the step at
#' which stabilitest reports the decision flips.
#'
#' @param result An `stv_result`.
#' @param mode `"light"` or `"dark"`.
#'
#' @return A ggplot.
#' @noRd
stv_plot_removal <- function(result, mode = "light") {
  col <- stv_plot_colours(mode)
  paths <- list(`Worst-case removal` = result@removal)
  if (!is.null(result@extreme) && nrow(result@extreme)) paths$`Extreme-value removal` <- result@extreme
  df <- do.call(rbind, lapply(names(paths), \(nm) transform(as.data.frame(paths[[nm]]), path = nm)))
  df$path <- factor(df$path, levels = names(paths))
  df$text <- paste0(
    df$path, "<br>", ifelse(df$k_removed == 0, "No observations removed",
      paste0("Step ", df$k_removed, ": removed ", df$label, " (row ", df$row, ")")),
    "<br>p = ", stv_tooltip_p(df$p_value)
  )

  # The step stabilitest marks as the first that no longer matches the decision
  flip <- df[df$path == "Worst-case removal" & !df$conclusion_match, , drop = FALSE]
  flip <- flip[order(flip$k_removed), , drop = FALSE][1, , drop = FALSE]
  flip <- flip[!is.na(flip$k_removed), , drop = FALSE]

  p <- ggplot2::ggplot(df, ggplot2::aes(.data$k_removed, .data$p_value, colour = .data$path, linetype = .data$path)) +
    ggplot2::geom_hline(yintercept = result@alpha, colour = col$muted, linetype = "dashed", linewidth = 0.5) +
    ggplot2::geom_line(linewidth = 0.8) +
    stv_tooltip_points(ggplot2::aes(text = .data$text), size = 1.6) +
    ggplot2::annotate("text", x = max(df$k_removed), y = result@alpha * 0.8,
      label = paste("alpha", format(result@alpha)), hjust = 1, colour = col$muted, size = 3.3) +
    ggplot2::scale_colour_manual(values = c(`Worst-case removal` = col$main, `Extreme-value removal` = col$second)) +
    ggplot2::scale_linetype_manual(values = c(`Worst-case removal` = "solid", `Extreme-value removal` = "22")) +
    ggplot2::scale_x_continuous("Observations removed", breaks = scales_int_breaks) +
    stv_p_axis("p-value") +
    stv_plot_theme(col)
  if (nrow(flip)) {
    p <- p +
      ggplot2::geom_point(data = flip, ggplot2::aes(.data$k_removed, .data$p_value), shape = 21, size = 4,
        stroke = 1, colour = col$marked, fill = NA, inherit.aes = FALSE) +
      ggplot2::annotate("text", x = flip$k_removed - 0.6, y = flip$p_value * 2.6,
        label = paste0("Decision flips after ", flip$k_removed), hjust = 1,
        colour = col$marked, size = 3.3)
  }
  p
}

#' Points that carry plotly tooltip and key aesthetics
#'
#' ggplot2 does not know the `text` and `key` aesthetics that plotly reads, and
#' warns about them; the static plot ignores them.
#'
#' @param mapping Aesthetics.
#' @param ... Passed to `ggplot2::geom_point()`.
#'
#' @return A ggplot2 layer.
#' @noRd
stv_tooltip_points <- function(mapping, ...) {
  suppressWarnings(ggplot2::geom_point(mapping, ...))
}

#' Whole-number axis breaks
#'
#' @param limits Axis limits.
#'
#' @return Integer breaks.
#' @noRd
scales_int_breaks <- function(limits) {
  b <- pretty(limits)
  unique(b[b == round(b) & b >= 0])
}

#' Influence plot
#'
#' The p-value with each observation left out (jackknife), against the
#' original p-value and the significance level. Observations stabilitest flags
#' as influential are marked; `selected` rows get a ring.
#'
#' @param result An `stv_result`.
#' @param selected Rows of the filtered data to ring.
#' @param mode `"light"` or `"dark"`.
#'
#' @return A ggplot.
#' @noRd
stv_plot_influence <- function(result, selected = integer(), mode = "light") {
  col <- stv_plot_colours(mode)
  df <- as.data.frame(result@jackknife)
  df$status <- factor(ifelse(df$influential %in% TRUE, "Influential (stabilitest)", "Not flagged"),
    levels = c("Not flagged", "Influential (stabilitest)"))
  df$text <- paste0(df$label, " (row ", df$row, ")<br>p without it = ", stv_tooltip_p(df$p_value),
    ifelse(df$influential %in% TRUE, "<br>Flagged as influential", ""))
  df$order <- seq_len(nrow(df))
  ring <- df[df$row %in% selected, , drop = FALSE]

  ggplot2::ggplot(df, ggplot2::aes(.data$order, .data$p_value)) +
    ggplot2::geom_hline(yintercept = result@alpha, colour = col$muted, linetype = "dashed", linewidth = 0.5) +
    ggplot2::geom_hline(yintercept = result@p_value, colour = col$main, linewidth = 0.5) +
    stv_tooltip_points(ggplot2::aes(colour = .data$status, text = .data$text, key = .data$row), size = 1.8) +
    ggplot2::geom_point(data = ring, ggplot2::aes(.data$order, .data$p_value), shape = 21, size = 4,
      stroke = 1, colour = col$fg, fill = NA, inherit.aes = FALSE) +
    ggplot2::labs(caption = paste0("Solid line: original p. Dashed line: alpha ", format(result@alpha), ".")) +
    ggplot2::scale_colour_manual(values = c(`Not flagged` = col$muted, `Influential (stabilitest)` = col$marked), drop = FALSE) +
    ggplot2::scale_x_continuous("Observation (left out)", breaks = scales_int_breaks) +
    stv_p_axis("p-value with it left out") +
    stv_plot_theme(col)
}

#' Bootstrap plot
#'
#' Distribution of the p-values of the bootstrap resamples, with the
#' significance level and the original p-value.
#'
#' @param result An `stv_result`.
#' @param mode `"light"` or `"dark"`.
#'
#' @return A ggplot.
#' @noRd
stv_plot_bootstrap <- function(result, mode = "light") {
  col <- stv_plot_colours(mode)
  df <- as.data.frame(result@bootstrap)
  df <- df[is.finite(df$p_value) & df$p_value > 0, , drop = FALSE]
  df$decision <- factor(ifelse(df$conclusion_match %in% TRUE, "Same decision", "Other decision"),
    levels = c("Same decision", "Other decision"))

  ggplot2::ggplot(df, ggplot2::aes(.data$p_value, fill = .data$decision)) +
    ggplot2::geom_histogram(bins = 40, colour = col$bg, linewidth = 0.2) +
    ggplot2::geom_vline(xintercept = result@alpha, colour = col$muted, linetype = "dashed", linewidth = 0.5) +
    ggplot2::geom_vline(xintercept = result@p_value, colour = col$fg, linewidth = 0.6) +
    ggplot2::scale_x_log10("p-value of the resample",
      labels = \(x) format(x, scientific = FALSE, drop0trailing = TRUE, trim = TRUE)) +
    ggplot2::scale_y_continuous("Resamples") +
    ggplot2::scale_fill_manual(values = c(`Same decision` = col$main, `Other decision` = col$muted), drop = FALSE) +
    ggplot2::labs(caption = paste0("Solid line: original p. Dashed line: alpha ", format(result@alpha), ".")) +
    stv_plot_theme(col)
}

#' Make a ggplot interactive
#'
#' @param p A ggplot.
#' @param source plotly event source id, or `NULL`.
#' @param mode `"light"` or `"dark"`.
#'
#' @return A plotly htmlwidget.
#' @noRd
stv_plotly <- function(p, source = NULL, mode = "light") {
  col <- stv_plot_colours(mode)
  w <- plotly::ggplotly(p, tooltip = "text", source = source %||% "A")
  w <- plotly::layout(
    w,
    paper_bgcolor = col$bg, plot_bgcolor = col$bg, font = list(color = col$fg),
    legend = list(orientation = "h", x = 0, y = 1.12, title = list(text = "")),
    hoverlabel = list(align = "left"),
    margin = list(t = 40)
  )
  plotly::config(w, displaylogo = FALSE, modeBarButtonsToRemove = c("lasso2d", "autoScale2d", "toggleSpikelines"))
}
