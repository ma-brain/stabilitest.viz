read_syn <- \(f) stv_read_file(system.file("extdata", "synthetic", f, package = "stabilitest.viz"), f)$data

# Each wrong scenario is refused before it reaches stabilitest, with a message
# that names the problem. `pattern` is matched against the problems.
wrong <- list(
  # Two groups, continuous
  list("location", "t.test", "binary_response.csv", list(outcome = "responder", group = "arm", levels = c("Active", "Control")),
    "only takes the values 0 and 1. Use Compare two proportions"),
  list("location", "t.test", "two_sample_continuous.csv", list(outcome = "outcome", group = "subject_id", levels = c("S001", "S002")),
    "identifies subjects"),
  list("location", "t.test", "two_sample_continuous.csv", list(outcome = "subject_id", group = "arm", levels = c("Active", "Control")),
    "identifies subjects"),
  list("location", "t.test", "two_sample_continuous.csv", list(outcome = "arm", group = "arm", levels = c("Active", "Control")),
    "must be numeric"),
  list("location", "wilcoxon", "paired_bp.csv", list(outcome = "after", group = "before", levels = c("146.9", "141.2")),
    "too many different values to define groups"),
  list("location", "t.test", "ancova_trial.csv", list(outcome = "change", group = "arm", levels = c("Low", "Low")),
    "two different groups"),
  # Paired
  list("location", "paired.t.test", "binary_response.csv", list(outcome = "responder", outcome2 = "responder"),
    "only takes the values 0 and 1"),
  list("location", "paired.t.test", "paired_bp.csv", list(outcome = "before", outcome2 = "before"),
    "must be different columns"),
  list("location", "paired.t.test", "two_sample_continuous.csv",
    list(paired_layout = "long", outcome = "outcome", group = "arm", levels = c("Active", "Control"), pair_id = "arm"),
    "used as both the group column and the pair identifier"),
  # Two proportions
  list("proportion", "fisher", "two_sample_continuous.csv", list(outcome = "outcome", group = "arm", levels = c("Active", "Control")),
    "is not a response"),
  list("proportion", "fisher", "binary_response.csv", list(outcome = "arm", group = "arm", levels = c("Active", "Control")),
    "used as both the outcome and the group column"),
  list("proportion", "chisq", "binary_response.csv", list(outcome = "responder", group = "arm", levels = c("Active", "Control"), success = "yes"),
    "does not occur"),
  # Equivalence
  list("tost", "mean", "binary_response.csv",
    list(outcome = "responder", group = "arm", levels = c("Active", "Control"), tost_type = "equivalence", margin = 0.1),
    "Use the risk difference or odds ratio endpoint"),
  list("tost", "prop", "two_sample_continuous.csv",
    list(outcome = "outcome", group = "arm", levels = c("Active", "Control"), tost_type = "equivalence", margin = 0.1),
    "is not a response"),
  list("tost", "prop", "binary_response.csv",
    list(outcome = "responder", group = "arm", levels = c("Active", "Control"), tost_type = "equivalence", margin = 5),
    "must be below 1"),
  list("tost", "or", "binary_response.csv",
    list(outcome = "responder", group = "arm", levels = c("Active", "Control"), tost_type = "noninferiority", margin = 0.8),
    "odds-ratio margin must be above 1"),
  # Linear model
  list("lm", NULL, "binary_response.csv", list(outcome = "responder", predictors = "arm", term = "armControl"),
    "Use Generalised linear model"),
  list("lm", NULL, "ancova_trial.csv", list(outcome = "change", predictors = c("arm", "subject_id"), term = "armLow"),
    "identifies subjects, so it cannot be a predictor"),
  list("lm", NULL, "ancova_trial.csv", list(outcome = "change", predictors = c("arm", "change"), term = "armLow"),
    "cannot be a predictor and also the outcome"),
  # Generalised linear model
  list("glm", "binomial", "two_sample_continuous.csv", list(outcome = "outcome", predictors = "arm", term = "armControl"),
    "binomial outcome must be 0/1"),
  list("glm", "poisson", "two_sample_continuous.csv", list(outcome = "outcome", predictors = "arm", term = "armControl"),
    "non-negative whole numbers"),
  list("glm", "poisson", "ancova_trial.csv",
    list(outcome = "age", predictors = "arm", term = "armLow", offset = "change"),
    "must be above 0 on every row"),
  # Cox
  list("cox", NULL, "ancova_trial.csv", list(time = "change", event = "arm", predictors = "age", term = "age"),
    "every value above 0"),
  list("cox", NULL, "survival_cox.csv", list(time = "time_months", event = "age", predictors = "arm", term = "armControl"),
    "must be 0/1"),
  list("cox", NULL, "survival_cox.csv", list(time = "time_months", event = "event", predictors = c("arm", "event"), term = "armControl"),
    "cannot be a predictor and also the event column")
)

test_that("wrong scenarios are refused with a clear message, for every method", {
  for (case in wrong) {
    data <- read_syn(case[[3]])
    spec <- do.call(stv_spec_new, c(list(case[[1]], case[[2]]), case[[4]]))
    problems <- stv_validate_spec(spec, data)
    label <- paste(case[[1]], case[[2]] %||% "", case[[3]], case[[5]])
    expect_true(any(grepl(case[[5]], problems, fixed = TRUE)), info = paste(label, "|", paste(problems, collapse = " / ")))
  }
})

test_that("a constant predictor and a Cox event column with no events are refused", {
  data <- read_syn("survival_cox.csv")
  data$site <- "A"
  spec <- stv_spec_new("cox", time = "time_months", event = "event", predictors = c("arm", "site"), term = "armControl")
  expect_match(stv_validate_spec(spec, data), "has a single value", all = FALSE)

  data$event <- 0L
  spec <- stv_spec_new("cox", time = "time_months", event = "event", predictors = "arm", term = "armControl")
  expect_match(stv_validate_spec(spec, data), "records no events", all = FALSE)
})

test_that("correct scenarios still pass, for every method", {
  skip_if_not_installed("survival")
  right <- list(
    list("location", "wilcoxon", "two_sample_continuous.csv", list(outcome = "outcome", group = "arm", levels = c("Active", "Control"))),
    list("location", "paired.t.test", "paired_bp.csv", list(outcome = "before", outcome2 = "after")),
    list("proportion", "fisher", "binary_response.csv", list(outcome = "responder", group = "arm", levels = c("Active", "Control"))),
    list("tost", "mean", "two_sample_continuous.csv",
      list(outcome = "outcome", group = "arm", levels = c("Active", "Control"), tost_type = "equivalence", margin = 8)),
    list("tost", "prop", "binary_response.csv",
      list(outcome = "responder", group = "arm", levels = c("Active", "Control"), tost_type = "equivalence", margin = 0.2)),
    list("lm", NULL, "ancova_trial.csv", list(outcome = "change", predictors = c("arm", "baseline"), term = "armLow")),
    list("glm", "poisson", "count_events.csv", list(outcome = "events", predictors = "arm", offset = "exposure_years", term = "armControl")),
    list("cox", NULL, "survival_cox.csv", list(time = "time_months", event = "event", predictors = c("arm", "age"), term = "armControl"))
  )
  for (case in right) {
    data <- read_syn(case[[3]])
    spec <- stv_spec_finalize(do.call(stv_spec_new, c(list(case[[1]], case[[2]]), case[[4]])), data)
    expect_identical(stv_validate_spec(spec, data), character(), info = paste(case[[1]], case[[3]]))
  }
})

test_that("Map choices follow the column roles", {
  k <- stv_column_kinds(read_syn("ancova_trial.csv"), ids = "subject_id")
  expect_setequal(k$measure, c("age", "baseline", "change"))
  expect_false("subject_id" %in% k$predictor)
  expect_identical(k$response, "arm")

  k2 <- stv_column_kinds(read_syn("binary_response.csv"))
  expect_identical(k2$binary, "responder")
  expect_false("responder" %in% k2$measure)
  expect_identical(k2$id, "subject_id")

  k3 <- stv_column_kinds(read_syn("count_events.csv"))
  expect_true(all(c("events", "exposure_years") %in% k3$positive | c("events", "exposure_years") %in% k3$count))
  expect_true("events" %in% k3$count)
})
