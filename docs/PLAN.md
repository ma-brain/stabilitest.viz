# stabilitest.viz — Development Plan (draft v0.1)

Companion R Shiny app for the `stabilitest` package (v0.6.5).
Separate package, built with **golem + bslib (Bootstrap 5)**. The app never re-implements
statistics: every number comes from `stabilitest` exported functions.

Status: **v0.8 — M0, M1 and M2 built (branches `m0-skeleton`, `m1-data-layer`, `m2-engine`).**

---

## 1. Goals

1. Let a user go from *data → robustness verdict → report* in under 2 minutes, without writing code.
2. Make the three stress tests (jackknife, worst-case removal, bootstrap same-decision rate) *visible and explorable*: which observations drive the result, and why.
3. Respect the package's **fail-closed calibration**: show a categorical label only when `stabilitest` returns one. Never infer a label from a score.
4. Produce reproducible output: every analysis exports the exact R code and a report.
5. Modern, calm UI: light/dark, responsive, keyboard-accessible, no "default Shiny" look.

Non-goals (v1): multi-user accounts, persistence database, editing data in-app, new statistical methods.

---

## 2. Coverage of the stabilitest API

| App "analysis family" | stabilitest call | Inputs the user maps |
|---|---|---|
| Two-sample location | `robustness_analysis(test_type = t.test / paired.t.test / wilcoxon / brunner_munzel)` | outcome, group (+ pair ID if paired) |
| Two-group proportions | `robustness_analysis(test_type = fisher / chisq / prop)` | binary outcome, group, `correct` |
| Equivalence / NI | `robustness_tost(endpoint = mean / prop / or)` | outcome, group, type, margin or δL/δU, `higher_is_better`, paired |
| Linear model / ANCOVA | `robustness_lm()` | formula builder (response, treatment, covariates), term |
| GLM | `robustness_glm()` | formula, family (binomial / poisson), offset, term, obs weights |
| Cox PH | `robustness_surv()` | time, event, covariates, term |

Common settings: `alpha`, `n_boot`, `max_removal_pct`, `influential_threshold`, `weights`, `seed`, `interpret`.

**Adapter layer.** `robustness_analysis` (nested `jackknife$results`, `extreme`) and
`robustness_model` (flat tibbles, `removed_rows`) differ. The app normalises both into one
internal result object (S7 class, e.g. `stv_result`) so every results module is written once.
*Recommendation:* later move a `tidy()`/`as_tibble()` accessor into `stabilitest` itself.

---

## 3. User experience

### 3.1 Layout
`bslib::page_navbar` with 4 top-level pages:

1. **Analyse** — the main workflow (sidebar + main area).
2. **Compare** — side-by-side comparison of analyses run in this session (e.g. Welch vs Wilcoxon vs Brunner–Munzel on the same data).
3. **Learn** — short explanation of each metric with illustrated examples (links to vignettes / R Journal article).
4. **About** — package versions, calibration registry status, citation.

Navbar right: **Guided / Expert mode switch**, dark-mode toggle (`input_dark_mode`), session history, help.

### 3.1a Two modes (three audiences)
- **Expert** (statisticians): all settings, technical wording, code tab open by default.
- **Guided** (clinical reviewers): plain-language step text, advanced settings hidden, a "What does this mean?" panel next to every result.
- **Public companion**: Guided mode + bundled example data preloaded + Learn page linking to the vignettes / R Journal article. Selected with `run_app(profile = "public")`, which sets `app_profile: public` in `golem-config.yml` and also forces Guided mode and disables uploads.

### 3.2 Analyse workflow (stepper in a collapsible sidebar)

1. **Data** — upload CSV / XLSX / SAS (`.xpt`, `.sas7bdat` via `haven`; labels shown as column tooltips) or pick a bundled example
   (`pain_trial`, `pain_ancova_trial`, `onc_response_trial`). Instant preview, column-type badges, missing-value summary, optional row filter.
   **ADaM-aware**: detects ADSL / BDS (ADEFF, ADLB…) / ADTTE structure by column names; offers PARAMCD / AVISIT / population-flag (SAFFL, ITTFL, EFFFL) filters; presets mapping (AVAL/CHG → outcome, TRT01P/TRTP → group, BASE → covariate, AVAL+CNSR → time/event for Cox, with CNSR inverted to event). One-row-per-subject check before running.
2. **Question** — pick analysis family via cards with a one-line plain-language description ("Is the difference in means fragile?"). Smart defaults from column types (binary outcome → proportions).
3. **Map variables** — drop-downs filtered by type; formula builder for models with live formula preview (`change ~ arm + baseline`); term picker populated from the fitted model.
4. **Settings** — basic (alpha, n_boot) visible; advanced (weights, max removal %, threshold, seed) in an accordion. Weight sliders sum-to-1 enforced. Warning when settings fall outside a validated calibration profile.
5. **Run** — `bslib::input_task_button` + `ExtendedTask` (async via `mirai`) so the UI never freezes; progress + cancel; runtime estimate from n × n_boot.

### 3.3 Results dashboard
- **Verdict strip** (value boxes): original p and decision, composite score (0–100 gauge), removal fragility index, bootstrap same-decision rate, jackknife stability.
- **Calibration banner**: either the validated label (e.g. Fisher "Fragile / Not fragile", with profile ID) or a neutral fail-closed message: "No validated cutoff for this method — score is numeric only." Never coloured red/green when there is no label.
- **Tabs**:
  - *Removal path* — p-value vs k removed (worst-case and extreme-value), alpha line, flip point annotated.
  - *Influence* — jackknife Δp per observation; click a point → highlights the row in the data table.
  - *Bootstrap* — p-value distribution with alpha and observed p; same-decision rate.
  - *Removed observations* — table of the greedy removal set, linked to the raw data.
  - *Interpretation* — the package's `interpret = TRUE` text.
  - *Code* — copyable, runnable R code reproducing the result exactly (incl. seed).
- Plot hover/click via interactive graphics; static ggplot versions used for the report.

### 3.4 Export
- Report: HTML, DOCX and PDF (Typst engine, no LaTeX needed in Docker) via one Quarto template (parameterised), including session info and package versions.
- Download: plots (PNG/SVG), result tables (CSV/XLSX), result object (RDS), R script.

### 3.5 UX principles
- Progressive disclosure (basic → advanced).
- Every metric has an info popover (`bslib::popover`) with a one-sentence definition.
- Validation messages inline next to the input (`shinyvalidate`), not modals.
- Empty states and skeleton loaders; no blank panels.
- Keyboard and screen-reader friendly; WCAG AA contrast in both themes.

---

## 4. Visual design
Brand spec: [`docs/brand/BRAND.md`](brand/BRAND.md) ("Clinical calm" palette, light/dark tokens, typography, logo concepts).

- `bslib::bs_theme(version = 5)` with a custom palette, Inter + JetBrains Mono, rounded cards, generous whitespace.
- Single SCSS file in `inst/app/www/` for overrides; design tokens as Sass variables.
- Plots take their light/dark colours from `stv_palette()` directly (`thematic` was dropped in M3, see the decisions log).
- Colour semantics: neutral by default; red/green reserved for validated labels only.

---

## 5. Architecture (golem)

```
stabilitest.viz/
├── DESCRIPTION            # Imports: stabilitest, golem, bslib, shiny, ...
├── R/
│   ├── app_ui.R  app_server.R  app_config.R  run_app.R
│   ├── mod_data_*.R        # upload, example data, preview
│   ├── mod_question.R      # analysis family selector
│   ├── mod_mapping_*.R     # per-family variable mapping
│   ├── mod_settings.R
│   ├── mod_run.R           # ExtendedTask / mirai
│   ├── mod_results_*.R     # verdict, removal, influence, bootstrap, tables, code
│   ├── mod_compare.R  mod_learn.R  mod_about.R
│   ├── fct_dispatch.R      # builds & calls the stabilitest function
│   ├── fct_adapter.R       # normalises results → stv_result (S7)
│   ├── fct_codegen.R       # reproducible R code
│   ├── fct_plots.R         # ggplot builders (shared by app and report)
│   └── utils_*.R
├── inst/
│   ├── app/www/            # scss, favicon, logo
│   ├── report/report.qmd   # Quarto template
│   └── golem-config.yml    # profiles default / production / public: app_profile, max_upload_mb, max_n_boot, workers
├── tests/testthat/         # unit + testServer() + shinytest2
├── docs/                   # this plan, decisions log, UX notes
└── dev/                    # golem dev scripts
```

Key rules:
- `fct_*` are pure functions, tested without Shiny.
- Modules communicate through one session-level `reactiveValues`/R6 store (data, spec, results, history).
- All `stabilitest` calls go through `fct_dispatch.R` — the single point to update when the package API changes.

---

## 6. Proposed dependencies
M0 ships only `bslib`, `bsicons`, `config`, `golem`, `rlang`, `sass`, `shiny`, `stabilitest`, `tibble`, `utils`. The rest are added per milestone: `S7` in M2 (approved), `mirai` in M2, `readxl`/`haven`/`shinyvalidate` in M1, `ggplot2`/`plotly`/`reactable` in M3, `quarto` in M4.

`stabilitest`, `golem`, `shiny` (≥ 1.8.1 for ExtendedTask), `bslib`, `bsicons`, `mirai`,
`ggplot2`, `plotly` (in-app interactivity; ggplot builders in `fct_plots.R` → `ggplotly()` / native traces where needed, static ggplot for reports), `reactable`, `shinyvalidate`,
`readxl`, `haven`, `S7`, `quarto`, `cli`; dev: `testthat`, `shinytest2`, `lintr`, `styler`.

---

## 7. Quality
- Unit tests for dispatch, adapter, codegen (round-trip: generated code reproduces app result bit-for-bit given seed).
- `testServer()` for each module; `shinytest2` snapshot for the happy path per analysis family.
- GitHub Actions: R CMD check + shinytest2.
- `lintr` + `styler` (tidyverse style).

---

## 8. Milestones (all six families in v1)

| # | Milestone | Content |
|---|---|---|
| M0 | Skeleton | golem package, theme, navbar, modes switch, config profiles, CI, examples load |
| M1 | Data layer | CSV/XLSX upload, preview, ADaM detection + filters + mapping presets |
| M2 | Engine | dispatch for all six families, S7 adapter, async run (mirai), codegen + round-trip tests |
| M3 | Results | verdict strip, calibration banner, removal / influence / bootstrap / removed-obs / interpretation / code tabs, linked brushing |
| M4 | Compare + reports | session history, compare view, Quarto export |
| M5 | Guided mode, Learn, polish | plain-language layer, public profile, accessibility pass |
| M6 | Release | Dockerfile (rocker/r-ver base), `run_app()` docs, v1.0 |

## 9. Risks
- **Runtime**: large `n_boot` on models (Cox) can take minutes → async + caps in config + estimated runtime.
- **API drift**: `stabilitest` is experimental → pin minimum version, single dispatch file, adapter tests.
- **Misinterpretation**: users may read a score as a verdict → fail-closed banner, neutral colours, wording reviewed against the package README.

---

## 10. Open questions (to decide)
1. ~~Audience~~ → decided.
2. ~~v1 scope~~ → decided.
3. ~~Data sources~~ → decided.
4. ~~Deployment~~ → decided.
5. ~~Interactive plots~~ → decided.
6. ~~Report formats~~ → decided.
7. ~~stabilitest source~~ → decided.
8. ~~Branding~~ → own brand, "Clinical calm". Logo B · balance selected.

---

## Decisions log
- 2026-10-26 — Audience: statisticians, clinical reviewers and public companion → Expert/Guided modes + `public` config profile.
- 2026-10-26 — v1 scope: all six analysis families.
- 2026-10-26 — Data input: CSV / XLSX, ADaM-aware mapping. SAS `.xpt` / `.sas7bdat` via `haven`.
- 2026-10-26 — Deployment: local `run_app()` and Docker / self-hosted.
- 2026-10-26 — Interactive plots: `plotly`; static ggplot for reports with a shared theme.
- 2026-10-26 — Reports: HTML, DOCX, PDF from one Quarto template.
- 2026-10-26 — Dependency: `Remotes: ma-brain/stabilitest`, `stabilitest (>= 0.6.5)`.
- 2026-10-26 — Branding: own logo + palette, "Clinical calm" direction, shared hex family with `stabilitest`.
- 2026-10-26 — Logo: concept B (balance). Typography: Inter + JetBrains Mono.
- 2026-10-26 — M0: default UI mode is Guided; the public profile forces Guided and hides the switch. Profile is chosen with `run_app(profile=)` (falls back to a valid `R_CONFIG_ACTIVE`, else `default`); config keys `app_profile`, `max_upload_mb`, `max_n_boot`, `workers`.
- 2026-10-26 — M0: example id `pain_trial` is `pain_treatment`/`pain_placebo` stacked into a long tibble (`subject_id`, `arm`, `change`; arm levels Treatment, Placebo). The PLAN's "pain_trial" is the package's documentation name, not a dataset.
- 2026-10-26 — M0: tables are plain HTML via `stv_html_table()`; `reactable` is deferred to M3. `utils` is declared in Imports for `read.csv`.
- 2026-10-26 — M0: the registry "validated" badge uses the primary colour, not green, since green/red are reserved for result labels.
- 2026-10-26 — M0: Sass declarations are set through `bs_theme()` variables (`body-bg`, `body-bg-dark`, ...); dark-mode primary buttons and switches are overridden in `stv.scss` because Bootstrap compiles `$primary` into components.
- 2026-10-26 — S7 approved for M2. Config key is `app_profile` (not `profile`); PLAN text corrected. Dependencies are added per milestone, see section 6.
- 2026-10-26 — Synthetic test data (CSV) generated by `dev/make_synthetic_data.R` into `inst/extdata/synthetic/`; see `docs/synthetic-data.md`.
- 2026-10-26 — M1: `readxl`, `haven` and `shinyvalidate` approved and added. Upload accepts .csv, .tsv, .txt, .xlsx, .xls, .xpt, .sas7bdat up to `max_upload_mb`; the profile limit is applied to `shiny.maxRequestSize` in an `onStart` hook. The public profile has no upload.
- 2026-10-26 — M1: ADaM kind is detected from column names (case-insensitive): ADTTE = AVAL + CNSR + PARAMCD, BDS = AVAL + PARAMCD, ADSL = USUBJID + treatment and no AVAL/PARAMCD. Default filters for BDS/ADTTE: first PARAMCD, last visit, ANL01FL = Y if present. Population flags are opt-in. Filters ignore values that are not in the data.
- 2026-10-26 — M1: mapping presets are suggestions shown in the Data step and stored in `store$suggestion`; the Map step that edits them arrives in M2. ADaM: outcome CHG (AVAL as alternative), group TRT01P/TRTP, covariate BASE, ADTTE time AVAL and event `1 - CNSR`. Other data: name and type heuristics.
- 2026-10-26 — M1: the one-row-per-subject check is informational in M1 and should block Run in M2. Data-quality badges use neutral colours.
- 2026-10-26 — M1: the synthetic CSV files are listed as bundled examples; row filter and SAS label tooltips appear in Expert mode and wherever labels exist.
- 2026-10-26 — M2: `mirai` approved (persistent daemons, real cancellation) and S7 for `stv_result`; `survival` is a Suggests, Cox needs it. `later` and `promises` are Suggests for tests only. `workers` in the profile sets the daemon count, started in the `onStart` hook.
- 2026-10-26 — M2: a spec becomes a plan of R language objects (`fct_dispatch.R`). The same plan is evaluated to run the analysis and printed as the reproducible code, so code and app cannot drift apart; tests check the printed code reproduces the app result with `identical()`.
- 2026-10-26 — M2: the app drops incomplete rows itself (complete cases on the columns used) and keeps a row map, so results link to rows of the filtered data shown in the Data step. Text predictors become factors in the prepared data (stabilitest needs this for glm and Cox).
- 2026-10-26 — M2: runs pin `RNGkind("Mersenne-Twister", "Inversion", "Rejection")` so a seed gives the same numbers in a daemon and in plain R; the line is part of the generated code.
- 2026-10-26 — M2: `stv_result` (S7) holds a label only when stabilitest returned one and its calibration is applicable; a validator enforces it. The banner is neutral ("No categorical label") otherwise. Green and red appear only with a returned label.
- 2026-10-26 — M2: the "validated Fisher settings" button applies alpha 0.05, 1000 resamples, weights 0/0.5/0.5 and 30% removal, the profile documented in the stabilitest README; a test compares the weights with stabilitest's frozen design. Eligibility is stabilitest's call.
- 2026-10-26 — M2: the one-row-per-subject check blocks Run (except paired data in long layout). Runs are limited by `max_n_boot` of the profile. Results are minimal in M2 (banner, five numbers, code); the tabbed explorer is M3.
- 2026-10-27 — Results score row: five equal boxes (composite, jackknife, removal fragility, bootstrap same-decision rate, p-value) with the same rows each. A red-to-green background by value was considered and rejected: it reads a verdict into scores that stabilitest cannot label (fail-closed rule). Each 0 to 100 value gets a meter in one brand colour whatever its value. The weights stabilitest returns (`result$weights`, now `stv_result@weights`) appear as a chip on each component, and a line under the row shows weights and components next to stabilitest's score. The fragility box shows `worstcase_fragility_component` (the value in the score) with the observation count as caption. Only a label stabilitest returned colours the banner and the composite box.
- 2026-10-27 — Question and Map outputs render while their panels are closed, so the suggested mapping applies when someone goes straight to Run.
- 2026-10-27 — Map: columns whose values are all different (subject IDs) are not offered as a group, and Map says so when no column can define groups. Data with no group column and two or more numeric measurements suggests the paired t-test; the user's own test choice wins after that. The app does not copy stabilitest's minimum group sizes (they differ by test); a stabilitest error is shown in its own words, without R's call.
- 2026-10-27 — Tighter scenarios for every method. `stv_column_kinds()` classifies columns by the roles they can take (identifier, constant, measurement, response, group, count, positive, non-negative, predictor). Map offers only fitting columns for each role, keeps the outcome and group apart, and says why when nothing fits. `stv_validate_spec()` applies the same rules before a run: no identifier in any role, no column in two roles, measurements must have more than two values (0/1 outcomes point to the proportion, risk-difference/odds-ratio or binomial options), responses must be 0/1 or a few categories, Poisson counts non-negative whole numbers, offsets and Cox times above 0, weights at least 0, Cox events must include at least one event, predictors must vary, a risk-difference margin must be below 1. These are structural checks only; minimum sample sizes stay with stabilitest. A table of wrong scenarios per method is in `tests/testthat/test-scenarios.R`.
- 2026-10-27 — Released v0.1.0 (M0 to M2). Changes are listed in `NEWS.md`.
- 2026-10-27 — M3: `ggplot2`, `plotly`, `reactable` and `thematic` approved and added to Imports.
- 2026-10-27 — stabilitest 0.6.6 released (PR #13): two-group removal paths name the removed unit. The app now requires `stabilitest (>= 0.6.6)`.
- 2026-10-27 — M3: `thematic` removed again. Its auto-theming only works inside `renderPlot()`, and the plots are plotly widgets that already take every colour from `stv_palette()` for the current light/dark mode.
- 2026-10-27 — M3: the Influence tab plots each leave-one-out p-value against a line at the original p, not Δp. The difference shows on the plot, and the app does no arithmetic on stabilitest's numbers. p-value axes are log scale.
- 2026-10-27 — M3: selection is one set of data rows shared by the Influence plot (click, or drag to select) and the Removed observations table (checkboxes). Rows the worst-case path did not remove have no table row and stay selected in the plot only. Plot builders in `R/fct_plots.R` return ggplots, so M4 reports can use them as static plots.
- 2026-10-27 — M4: `quarto`, `svglite` and `writexl` approved and added to Imports (report rendering, SVG plots, XLSX tables).
- 2026-10-27 — M4: the Compare page is the session history. Every run is kept with its data label and row count (`stv_store_add_result()`); there is no separate history menu in the navbar. Compare shows the same summary (`stv_result_summary()`) as the report, and warns when the analyses used different data.
- 2026-10-27 — M4: the report template (`inst/report/report.qmd`) has no code chunks. The app fills it with Markdown text and PNG plots and Quarto renders it, so rendering needs only the Quarto CLI (no knitr, and no installed copy of this package). PDF uses Typst. Reports render in the Shiny process on download (1 to 3 s). Where Quarto is not installed the Export tab says so.
- 2026-10-27 — M4: downloads sit in a seventh results tab, Export. The RDS download is the object stabilitest returned (`result@raw`), usable without the app.
