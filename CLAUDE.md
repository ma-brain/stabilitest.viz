# CLAUDE.md

stabilitest.viz is an R Shiny app (golem + bslib, Bootstrap 5.3), the companion to
the R package stabilitest (`../stabilitest`, GitHub `ma-brain/stabilitest`). It is a
separate package. Source of truth: `docs/PLAN.md`. Brand: `docs/brand/BRAND.md`.

## Hard rules
- **Never re-implement statistics.** Every number comes from stabilitest's exported
  functions. All stabilitest calls go through `R/fct_dispatch.R`.
- **stabilitest changes go upstream, not into the app.** A stabilitest bug or gap
  found while building the app (M3 to M5) is fixed in `../stabilitest`: a branch
  from its `main`, a test that no numbers change, a version bump, a PR (squash
  merge), a tag and a release. Then raise `stabilitest (>= x.y.z)` here and
  record it in `docs/upstream-requests.md`.
- **Fail-closed calibration.** Show a categorical label only when stabilitest returns
  one (today: eligible, significant Fisher). Otherwise show the neutral "numeric only,
  no validated cutoff" message. Never infer a label from a score. Green/red only for
  validated labels.
- Call the resampling component the **"bootstrap same-decision rate"** in the UI, never
  "reproducibility".
- Modern R: native pipe, tidyverse style, S7 for the internal result class, explicit
  `pkg::` calls, roxygen2 docs. `fct_*` are pure functions; `mod_*` are Shiny modules.
- All docs go in `docs/`. Update the decisions log in `docs/PLAN.md` when deciding.
- Ask before adding dependencies beyond DESCRIPTION, or if PLAN.md contradicts the
  stabilitest code.

## Commands
- Run: `Rscript dev/run_dev.R [public]` or `stabilitest.viz::run_app(profile = "public")`
- Docs: `Rscript -e 'devtools::document()'`
- Test: `Rscript -e 'devtools::test()'`
- Check: `Rscript -e 'devtools::check()'` (must stay at 0 errors, 0 warnings, 0 notes)
- Profiles (`inst/golem-config.yml`): default, production, public (Guided only,
  uploads off, example preloaded). `R_CONFIG_ACTIVE` is honoured if valid.

## Layout
`R/app_*.R` entry; `R/fct_theme.R` (`stv_palette()`, `stv_theme()`); `R/fct_store.R`
(session store); `R/fct_examples.R`; `R/fct_read.R` (file reading); `R/fct_adam.R` (ADaM detection, filters, mapping suggestions); `R/fct_spec.R` (families, validation); `R/fct_dispatch.R` (the only place that calls stabilitest); `R/fct_adapter.R` (`stv_result`); `R/fct_codegen.R`; `R/fct_plots.R` (ggplot builders, `stv_plotly()`); `R/fct_summary.R` (result summary for Compare and report); `R/fct_export.R` (downloads, report); `inst/report/report.qmd`; `R/mod_*.R` pages; SCSS in `inst/app/scss/stv.scss`;
assets in `inst/app/www/`.

## Milestones
- M0 Skeleton: **done** (branch `m0-skeleton`)
- M1 Data layer: **done** (branch `m1-data-layer`): CSV/TSV/XLSX/XPT/SAS7BDAT upload, ADaM detection, filters, one-row-per-subject check, suggested mapping
- M2 Engine: **done** (branch `m2-engine`): spec to plan to run (mirai) to `stv_result` (S7), generated code, Question/Map/Settings/Run steps
- M3 Results: **done** (branch `m3-results`): explorer tabs (`R/fct_plots.R`, `R/mod_results.R`), linked selection
- M4 Compare + reports: **done** (branch `m4-compare-reports`): session history and Compare page, Export tab, Quarto report
- M5 Guided/Learn/polish: **done** (branch `m5-guided-learn`): plain-language panel, metric popovers, Learn links, citation, accessibility pass
- M6 Release: not started. See `docs/PLAN.md` section 8.
