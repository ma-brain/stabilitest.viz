#' Brand palette
#'
#' Design tokens of the "Clinical calm" brand (`docs/brand/BRAND.md`).
#'
#' @param mode `"light"` or `"dark"`.
#'
#' @return A named list of hex colours. Mode-dependent theme tokens (`bg`,
#'   `surface`, `fg`, `muted`, `primary`, `accent`, `border`) and the semantic
#'   result colours (`not_fragile`, `fragile`, `numeric_only`) follow `mode`.
#'   `core` holds the mode-independent brand colours and `series` the data-viz
#'   series order.
#'
#' @details
#' `not_fragile` and `fragile` must only be used for a categorical label that
#' stabilitest returned. Everything else is neutral (`numeric_only`).
#'
#' @export
#'
#' @examples
#' stv_palette("light")$primary
#' stv_palette("dark")$bg
stv_palette <- function(mode = c("light", "dark")) {
  mode <- rlang::arg_match(mode)

  core <- c(
    ink = "#14323A",
    teal = "#1F6F78",
    teal_light = "#5FA8A6",
    mist = "#E3EEEC",
    paper = "#F7F5F0",
    sand = "#D9CFBF",
    amber = "#C8843A",
    amber_text = "#9A5E22"
  )
  series <- c("#1F6F78", "#C8843A", "#5FA8A6", "#14323A", "#B8A88F")

  tokens <- switch(
    mode,
    light = list(
      bg = "#F7F5F0",
      surface = "#FFFFFF",
      fg = "#14323A",
      muted = "#5A6B70",
      primary = "#1F6F78",
      accent = "#9A5E22",
      border = "#D9CFBF",
      not_fragile = "#2F7D5B",
      fragile = "#B4483C",
      numeric_only = "#5E6E73"
    ),
    dark = list(
      bg = "#0F1E22",
      surface = "#16292E",
      fg = "#E8EEEC",
      muted = "#9DB0B2",
      primary = "#6CB8B3",
      accent = "#E0A262",
      border = "#2A4148",
      not_fragile = "#6BC49A",
      fragile = "#E8857A",
      numeric_only = "#9DB0B2"
    )
  )

  c(tokens, list(core = core, series = series))
}

#' Brand theme
#'
#' A Bootstrap 5.3 `bslib::bs_theme()` built from `stv_palette()`. Light and
#' dark values are set through the Bootstrap Sass variables (`body-bg`,
#' `body-bg-dark`, ...) so that `bslib::input_dark_mode()` switches cleanly.
#' Extra rules live in `inst/app/scss/stv.scss`.
#'
#' @param local_fonts Download and bundle Inter and JetBrains Mono with
#'   `bslib::font_google()`. Needs network access the first time and is cached
#'   afterwards. Set to `FALSE` to fall back to system fonts.
#'
#' @return A `bslib::bs_theme()` object.
#' @export
#'
#' @examples
#' \donttest{
#' stv_theme(local_fonts = FALSE)
#' }
stv_theme <- function(local_fonts = TRUE) {
  light <- stv_palette("light")
  dark <- stv_palette("dark")

  if (local_fonts) {
    base_font <- bslib::font_google("Inter", wght = "400..700", local = TRUE)
    code_font <- bslib::font_google("JetBrains Mono", wght = "400..600", local = TRUE)
  } else {
    base_font <- bslib::font_collection("system-ui", "-apple-system", "Segoe UI", "sans-serif")
    code_font <- bslib::font_collection("ui-monospace", "SFMono-Regular", "Menlo", "monospace")
  }

  theme <- bslib::bs_theme(
    version = 5,
    primary = light$primary,
    secondary = light$muted,
    success = light$not_fragile,
    danger = light$fragile,
    warning = light$core[["amber"]],
    info = light$core[["teal_light"]],
    base_font = base_font,
    heading_font = base_font,
    code_font = code_font,
    "font-weight-base" = 400,
    "headings-font-weight" = 600,
    "enable-rounded" = TRUE,
    "border-radius" = "0.5rem",
    "border-radius-lg" = "0.75rem",
    # Light mode
    "body-bg" = light$bg,
    "body-color" = light$fg,
    "body-secondary-color" = light$muted,
    "body-tertiary-bg" = light$core[["mist"]],
    "border-color" = light$border,
    "link-color" = light$primary,
    # Dark mode (Bootstrap 5.3 `*-dark` variables)
    "body-bg-dark" = dark$bg,
    "body-color-dark" = dark$fg,
    "body-secondary-color-dark" = dark$muted,
    "body-tertiary-bg-dark" = dark$surface,
    "border-color-dark" = dark$border,
    "link-color-dark" = dark$primary,
    "link-hover-color-dark" = dark$fg
  )

  theme <- bslib::bs_add_variables(
    theme,
    .where = "declarations",
    "stv-surface-light" = light$surface,
    "stv-surface-dark" = dark$surface,
    "stv-muted-light" = light$muted,
    "stv-muted-dark" = dark$muted,
    "stv-accent-light" = light$accent,
    "stv-accent-dark" = dark$accent,
    "stv-primary-light" = light$primary,
    "stv-primary-dark" = dark$primary,
    "stv-not-fragile-light" = light$not_fragile,
    "stv-not-fragile-dark" = dark$not_fragile,
    "stv-fragile-light" = light$fragile,
    "stv-fragile-dark" = dark$fragile,
    "stv-numeric-only-light" = light$numeric_only,
    "stv-numeric-only-dark" = dark$numeric_only,
    "stv-mist" = light$core[["mist"]],
    "stv-ink" = light$core[["ink"]]
  )

  bslib::bs_add_rules(theme, sass::sass_file(app_sys("app", "scss", "stv.scss")))
}
