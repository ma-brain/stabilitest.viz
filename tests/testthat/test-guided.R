guided_result <- function(file, family, test, outcome, settings = list(n_boot = 40, seed = 4)) {
  data <- stv_read_file(system.file("extdata", "synthetic", file, package = "stabilitest.viz"), "x.csv")$data
  spec <- stv_spec_new(family, test, outcome = outcome, group = "arm", levels = c("Active", "Control"),
    settings = settings)
  plan <- stv_plan(spec)
  out <- stv_run_plan(plan, data)
  stv_as_result(out$raw, spec, plan, out$prepared, out$row_map)
}

test_that("plain-language text restates stabilitest's numbers", {
  r <- guided_result("two_sample_continuous.csv", "location", "t.test", "outcome")
  m <- r@metrics
  text <- stv_meaning_text(r)
  expect_length(text, 6L)
  expect_match(text[1], stv_format_p(r@p_value), fixed = TRUE)
  expect_match(text[2], paste("Removing", m$worstcase_fragility_k, "of the", r@n), fixed = TRUE)
  expect_match(text[3], paste0(stv_format_number(m$jackknife_conclusion_stability, 0), "%"), fixed = TRUE)
  expect_match(text[4], paste0(stv_format_number(m$bootstrap_same_decision_rate, 0), "%"), fixed = TRUE)
})

test_that("without a label the plain-language text does not read the score", {
  r <- guided_result("two_sample_continuous.csv", "location", "t.test", "outcome")
  expect_true(is.na(r@label))
  text <- paste(stv_meaning_text(r), collapse = " ")
  expect_match(text, "no validated cutoff", fixed = TRUE)
  expect_match(text, "is not called fragile or robust", fixed = TRUE)
  # "fragile" and "robust" appear only in that sentence
  expect_identical(lengths(regmatches(text, gregexpr("fragile|robust", text))), 2L)
})

test_that("a returned label is stated as stabilitest's", {
  r <- guided_result("binary_response_eligible.csv", "proportion", "fisher", "responder",
    settings = utils::modifyList(stv_fisher_profile_settings(), list(seed = 1)))
  skip_if(is.na(r@label), "stabilitest returned no label for this example")
  expect_match(paste(stv_meaning_text(r), collapse = " "), paste0("“", r@label, "”"), fixed = TRUE)
})

test_that("every score box has a definition and none says reproducibility", {
  r <- guided_result("two_sample_continuous.csv", "location", "t.test", "outcome")
  html <- as.character(stv_results_card(r))
  for (title in c("Composite score", "Jackknife stability", "Removal fragility", "Bootstrap same-decision rate",
    "Original p-value")) {
    expect_match(html, paste("About", title), fixed = TRUE)
  }
  all_text <- paste(c(unlist(stv_glossary()), stv_meaning_text(r)), collapse = " ")
  expect_false(grepl("reproducib", all_text, ignore.case = TRUE))
})

test_that("Guided shows the meaning panel, Expert opens the code", {
  r <- guided_result("two_sample_continuous.csv", "location", "t.test", "outcome")
  expect_match(as.character(stv_meaning_ui(r)), "What does this mean?", fixed = TRUE)
  expert <- as.character(stv_explorer_ui(shiny::NS("x"), r, selected = "Code"))
  expect_match(expert, "data-value=\"Code\"[^>]*class=\"[^\"]*active|class=\"[^\"]*active[^\"]*\"[^>]*data-value=\"Code\"")
})

test_that("the default resamples fit the profile's limit, so Guided mode can run", {
  for (profile in c("default", "public")) {
    store <- stv_store(profile)
    limit <- stv_config("max_n_boot", profile)
    shiny::testServer(mod_settings_server, args = list(store = store, family = shiny::reactive("location")), {
      settings <- session$returned()
      expect_lte(settings$n_boot, limit)
      expect_identical(settings$n_boot, min(stv_default_settings()$n_boot, limit))
    })
  }
})
