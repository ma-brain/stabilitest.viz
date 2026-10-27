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
