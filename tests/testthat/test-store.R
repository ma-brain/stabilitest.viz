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

test_that("ADaM data get default filters and a suggestion in the store", {
  store <- stv_store("default")
  shiny::testServer(mod_data_server, args = list(store = store), {
    session$setInputs(example = "syn_adeff")
    expect_identical(store$adam$kind, "BDS")
    expect_identical(nrow(store$raw_data), 300L)
    expect_identical(nrow(store$data), 100L)
    expect_true(store$checks$ok)
    expect_true("CHG" %in% store$suggestion$column)

    session$setInputs(avisit = "")
    expect_identical(nrow(store$data), 300L)
    expect_false(store$checks$ok)
    expect_identical(store$spec, list())
  })
})

test_that("uploading a file fills the store", {
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path))
  writeLines(c("subject_id,arm,y", "s1,a,1", "s2,b,2", "s3,a,3"), path)
  store <- stv_store("default")
  shiny::testServer(mod_data_server, args = list(store = store), {
    session$setInputs(file = data.frame(name = "mine.csv", size = file.size(path), type = "text/csv", datapath = path))
    expect_identical(store$data_label, "mine.csv")
    expect_identical(dim(store$data), c(3L, 3L))
    expect_identical(store$checks$n_subjects, 3L)
  })
})

test_that("an unreadable upload shows an inline message and keeps the old data", {
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path))
  writeLines("a,b", path)
  store <- stv_store("default")
  shiny::testServer(mod_data_server, args = list(store = store), {
    session$setInputs(example = "pain_trial")
    session$setInputs(file = data.frame(name = "bad.csv", size = file.size(path), type = "text/csv", datapath = path))
    expect_match(output$status$html, "no rows")
    expect_identical(store$data_id, "pain_trial")
  })
})

test_that("an oversized or unsupported upload is rejected before reading", {
  path <- tempfile(fileext = ".pdf")
  on.exit(unlink(path))
  writeLines("x", path)
  store <- stv_store("default")
  shiny::testServer(mod_data_server, args = list(store = store), {
    session$setInputs(file = data.frame(name = "a.pdf", size = 10, type = "application/pdf", datapath = path))
    expect_null(store$data)
  })
})

test_that("the public profile has no upload", {
  expect_false(grepl("Upload a file", as.character(mod_data_ui("d", public = TRUE))))
  expect_true(grepl("Upload a file", as.character(mod_data_ui("d", public = FALSE))))
})

test_that("start-up hook applies the upload limit", {
  old <- options(shiny.maxRequestSize = NULL)
  on.exit(options(old))
  stv_on_start("default")()
  expect_equal(getOption("shiny.maxRequestSize"), 50 * 1024^2)
})
