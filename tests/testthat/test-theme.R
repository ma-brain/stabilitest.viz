test_that("palette follows the brand tokens", {
  expect_identical(stv_palette("light")$primary, "#1F6F78")
  expect_identical(stv_palette("dark")$primary, "#6CB8B3")
  expect_identical(stv_palette("dark")$bg, "#0F1E22")
  expect_error(stv_palette("sepia"))
})

test_that("theme builds with light and dark variables", {
  # Google Fonts are downloaded; skip_if_offline() needs curl
  skip_if_not_installed("curl")
  skip_if_offline()
  theme <- stv_theme()
  expect_s3_class(theme, "bs_theme")
  vars <- bslib::bs_get_variables(
    theme,
    c("primary", "body-bg", "body-bg-dark", "body-color-dark", "border-color-dark")
  )
  expect_identical(unname(vars[["primary"]]), "#1F6F78")
  expect_identical(unname(vars[["body-bg"]]), "#F7F5F0")
  expect_identical(unname(vars[["body-bg-dark"]]), "#0F1E22")
  expect_identical(unname(vars[["body-color-dark"]]), "#E8EEEC")
  expect_identical(unname(vars[["border-color-dark"]]), "#2A4148")
})

test_that("theme compiles without web fonts", {
  theme <- stv_theme(local_fonts = FALSE)
  expect_identical(unname(bslib::bs_get_variables(theme, "primary")), "#1F6F78")
  expect_no_error(bslib::bs_theme_dependencies(theme))
})

test_that("text colours meet WCAG AA contrast in both themes", {
  luminance <- function(hex) {
    rgb <- grDevices::col2rgb(hex)[, 1] / 255
    lin <- ifelse(rgb <= 0.03928, rgb / 12.92, ((rgb + 0.055) / 1.055)^2.4)
    sum(c(0.2126, 0.7152, 0.0722) * lin)
  }
  contrast <- function(a, b) {
    l <- sort(c(luminance(a), luminance(b)), decreasing = TRUE)
    (l[1] + 0.05) / (l[2] + 0.05)
  }
  for (mode in c("light", "dark")) {
    pal <- stv_palette(mode)
    # Body text, secondary text, links and buttons, accents and label text
    for (fg in c("fg", "muted", "primary", "accent", "not_fragile", "fragile", "numeric_only")) {
      for (bg in c("bg", "surface")) {
        expect_gte(contrast(pal[[fg]], pal[[bg]]), 4.5, label = paste(mode, fg, "on", bg))
      }
    }
    # Plot lines and points against the plot background (non-text, 3:1)
    for (s in stv_series_colours(3, mode)) {
      expect_gte(contrast(s, pal$surface), 3, label = paste(mode, "series", s))
    }
  }
})

test_that("the markup keeps the accessibility fixes found with axe-core", {
  # One level-one heading for the app, and the script that fixes bslib markup
  expect_match(as.character(app_ui(NULL, profile = "default")), '<h1 class="visually-hidden">stabilitest.viz</h1>',
    fixed = TRUE)
  expect_match(as.character(shiny::tagList(golem_add_external_resources()$children)), "www/stv-a11y.js", fixed = TRUE)
  expect_true(file.exists(app_sys("app", "www", "stv-a11y.js")))

  # Scrolling tables can be reached and are named
  table <- as.character(stv_html_table(data.frame(a = 1), caption = "Versions"))
  expect_match(table, 'tabindex="0"', fixed = TRUE)
  expect_match(table, 'aria-label="Versions"', fixed = TRUE)

  # Export selects have labels for screen readers
  export <- as.character(stv_export_ui(shiny::NS("x")))
  for (label in c("Report format", "Plot", "Table")) {
    expect_match(export, sprintf('<span class="visually-hidden">%s</span>', label), fixed = TRUE)
  }
})
