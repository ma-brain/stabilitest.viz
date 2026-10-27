# Screenshots for the user's guide (docs/manuals/images), taken from the app
# running the bundled examples in the light theme.
#
#   Rscript dev/screenshots.R
#
# Needs chromote and a Chrome-based browser. Set CHROMOTE_CHROME to the
# browser's executable when it is not found automatically.

out <- file.path("docs", "manuals", "images")
dir.create(out, showWarnings = FALSE, recursive = TRUE)

port <- httpuv::randomPort()
app <- callr::r_bg(
  function(port, path) {
    pkgload::load_all(path, quiet = TRUE)
    shiny::runApp(run_app(profile = "default"), port = port, launch.browser = FALSE)
  },
  args = list(port, normalizePath(".")),
  stdout = tempfile("stv-app-"), stderr = "2>&1"
)
on.exit(app$kill(), add = TRUE)

b <- chromote::ChromoteSession$new()
on.exit(b$close(), add = TRUE, after = FALSE)

js <- function(code) invisible(b$Runtime$evaluate(code, returnByValue = TRUE)$result$value)
value <- function(code) b$Runtime$evaluate(code, returnByValue = TRUE)$result$value
wait <- function(seconds) Sys.sleep(seconds)

wait_for <- function(code, timeout = 60) {
  start <- Sys.time()
  while (!isTRUE(value(code))) {
    if (difftime(Sys.time(), start, units = "secs") > timeout) stop("Timed out waiting for: ", code)
    wait(0.5)
  }
}

open_app <- function(width = 1400, height = 900) {
  b$Emulation$setDeviceMetricsOverride(width = width, height = height, deviceScaleFactor = 2, mobile = FALSE)
  b$Page$navigate(sprintf("http://127.0.0.1:%d", port))
  wait_for("!!document.querySelector('#analyse-data-example')", 90)
  wait(2)
  js("document.documentElement.setAttribute('data-bs-theme', 'light')")
}

# Fit the viewport to the page, so element screenshots are not cut off
fit <- function(width = 1400) {
  height <- value("document.documentElement.scrollHeight")
  b$Emulation$setDeviceMetricsOverride(width = width, height = height, deviceScaleFactor = 2, mobile = FALSE)
  wait(1)
}

shot <- function(name, selector) {
  fit()
  b$screenshot(file.path(out, paste0(name, ".png")), selector = selector, scale = 1)
  message("saved ", name)
}

step <- function(name) {
  js(sprintf("document.querySelector('[data-value=\"%s\"] .accordion-button.collapsed')?.click()", name))
  wait(1)
}
example <- function(id) {
  js(sprintf("var e = $('#analyse-data-example'); e.val('%s'); e.trigger('change')", id))
  wait(2)
}
family <- function(id) {
  step("question")
  js(sprintf("$('input[name=\"analyse-question-family\"][value=%s]').click()", id))
  wait(1.5)
}
run <- function() {
  step("run")
  js("$('#analyse-run-run').click()")
  wait_for("!!document.querySelector('.stv-score-grid')", 120)
  wait(3)
}
tab <- function(label) {
  js(sprintf(
    "[...document.querySelectorAll('.nav-underline .nav-link')].find(a => a.textContent.trim() == '%s').click()",
    label
  ))
  wait(2.5)
}
page <- function(label) {
  js(sprintf(
    "[...document.querySelectorAll('.navbar .nav-link')].find(a => a.textContent.trim() == '%s').click()",
    label
  ))
  wait(3)
}

sidebar <- ".bslib-sidebar-layout > .sidebar .accordion"
explorer <- ".card:has(.nav-underline)"

# Data, question, map, settings
open_app()
example("pain_trial")
shot("data-preview", ".bslib-sidebar-layout > .main .card")
step("data")
shot("step-data", sidebar)
family("location")
shot("step-question", sidebar)
step("map")
shot("step-map", sidebar)
step("settings")
shot("step-settings", sidebar)

# Results in Guided mode
run()
shot("results-score", ".card:has(.stv-score-grid)")
shot("results-meaning", ".stv-meaning")
for (t in c("Removal path", "Influence", "Bootstrap", "Removed observations", "Interpretation", "Code", "Export")) {
  tab(t)
  shot(paste0("tab-", gsub(" ", "-", tolower(t))), explorer)
}

# A labelled result: the validated Fisher settings on eligible data
open_app()
example("syn_binary_eligible")
family("proportion")
step("settings")
js("$('#analyse-settings-use_fisher_profile').click()")
wait(1.5)
run()
shot("results-label", ".card:has(.stv-score-grid)")

# Compare: three tests on the same data
open_app()
example("syn_two_sample")
family("location")
run()
for (test in c("wilcoxon", "brunner_munzel")) {
  step("question")
  js(sprintf(
    "var e = $('#analyse-question-test'); e[0].selectize ? e[0].selectize.setValue('%s') : (e.val('%s'), e.trigger('change'))",
    test, test
  ))
  wait(1.5)
  js("document.querySelector('.stv-score-grid')?.closest('.card').remove()")
  run()
}
page("Compare")
shot("compare", ".stv-page:has(#compare-page)")

# Expert mode
page("Analyse")
js("$('#expert_mode').click()")
wait(2)
step("settings")
shot("step-settings-expert", sidebar)

page("Learn")
shot("learn", ".stv-learn")
page("About")
shot("about", ".tab-pane.active .stv-page")
