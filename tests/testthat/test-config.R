test_that("config profiles resolve", {
  expect_identical(stv_config("app_profile", "default"), "default")
  expect_identical(stv_config("app_profile", "production"), "production")
  expect_identical(stv_config("app_profile", "public"), "public")

  expect_gt(stv_config("max_upload_mb", "default"), 0)
  expect_equal(stv_config("max_upload_mb", "public"), 0)
  expect_lt(stv_config("max_n_boot", "public"), stv_config("max_n_boot", "default"))
  expect_true(is.numeric(stv_config("workers", "production")))
})

test_that("only the public profile is public", {
  expect_false(stv_is_public("default"))
  expect_false(stv_is_public("production"))
  expect_true(stv_is_public("public"))
})

test_that("profile resolution validates and falls back", {
  expect_error(stv_resolve_profile("staging"))
  withr_env <- Sys.getenv("R_CONFIG_ACTIVE", unset = NA)
  on.exit(if (is.na(withr_env)) Sys.unsetenv("R_CONFIG_ACTIVE") else Sys.setenv(R_CONFIG_ACTIVE = withr_env))

  Sys.setenv(R_CONFIG_ACTIVE = "shinyapps")
  expect_identical(stv_resolve_profile(NULL), "default")
  Sys.setenv(R_CONFIG_ACTIVE = "public")
  expect_identical(stv_resolve_profile(NULL), "public")
})

test_that("run_app returns an app object", {
  # Google Fonts are downloaded; skip_if_offline() needs curl
  skip_if_not_installed("curl")
  skip_if_offline()
  expect_s3_class(run_app(), "shiny.appobj")
  expect_s3_class(run_app(profile = "public"), "shiny.appobj")
  expect_error(run_app(profile = "staging"))
})
