local_profile <- function(profile, env = parent.frame()) {
  old <- Sys.getenv("R_CONFIG_ACTIVE", unset = NA)
  Sys.setenv(R_CONFIG_ACTIVE = profile)
  withr_restore <- function() {
    if (is.na(old)) Sys.unsetenv("R_CONFIG_ACTIVE") else Sys.setenv(R_CONFIG_ACTIVE = old)
  }
  do.call(on.exit, list(quote(withr_restore()), add = TRUE), envir = env)
  assign("withr_restore", withr_restore, envir = env)
}

test_that("mode follows the switch outside the public profile", {
  local_profile("default")
  shiny::testServer(app_server, {
    expect_identical(shiny::isolate(store$mode), "guided")
    session$setInputs(expert_mode = TRUE)
    expect_identical(store$mode, "expert")
    session$setInputs(expert_mode = FALSE)
    expect_identical(store$mode, "guided")
  })
})

test_that("the public profile forces Guided mode", {
  local_profile("public")
  shiny::testServer(app_server, {
    session$setInputs(expert_mode = TRUE)
    expect_identical(store$mode, "guided")
  })
})

test_that("the public UI has no mode switch", {
  expect_false(grepl("expert_mode", as.character(app_ui(NULL, "public"))))
  expect_true(grepl("expert_mode", as.character(app_ui(NULL, "default"))))
})

test_that("picking an example fills the store", {
  store <- stv_store("default")
  shiny::testServer(mod_data_server, args = list(store = store), {
    session$setInputs(example = "onc_response_trial")
    expect_identical(store$data_id, "onc_response_trial")
    expect_identical(dim(store$data), c(120L, 3L))

    session$setInputs(example = "")
    expect_null(store$data)
  })
})

test_that("the public profile preloads an example", {
  store <- stv_store("public")
  shiny::testServer(mod_data_server, args = list(store = store), {
    session$setInputs(example = stv_examples()$id[[1]])
    expect_identical(dim(store$data), c(55L, 3L))
  })
})

test_that("replacing data clears the spec but keeps history", {
  store <- stv_store("default")
  shiny::isolate({
    store$spec <- list(alpha = 0.05)
    stv_store_add_result(store, list(id = 1))
    stv_store_set_data(store, tibble::tibble(x = 1:3), "toy", "toy")
    expect_identical(store$spec, list())
    expect_length(store$history, 1)
    expect_error(stv_store_set_data(store, "not a data frame"))
  })
})
