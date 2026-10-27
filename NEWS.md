# stabilitest.viz 1.0.1

Manuals for each release, a note on the validated Fisher settings in the
public profile, and a fix to the Influence tab hint.

* Manuals: a user's guide, technical documentation and a two-page executive
  summary, attached as PDFs to each release from now on (`docs/manuals/`).
* In the `public` profile, which allows at most 500 bootstrap resamples, a
  Fisher exact test now shows a note that the validated settings (1000
  resamples) are out of reach there and results are numeric only, instead of
  a button that set more resamples than the profile allows.
* The Influence tab shows its "click a point" hint until observations are
  selected, instead of an empty table.

# stabilitest.viz 1.0.0

The complete app (milestones M3 to M6 of `docs/PLAN.md`): explore a result,
compare analyses, export a report, a plain-language Guided mode and a Docker
image. Every number still comes from stabilitest, now 0.6.6 or later.

## Results explorer

* Tabs below the score: removal path, influence, bootstrap, removed
  observations, interpretation, code and export. Plots are interactive and
  follow the light/dark switch.
* Selecting observations in the influence plot or the removed-observations
  table selects them in both. Each removal step links to the data row it
  removed, for every method (stabilitest 0.6.6 names the removed units).

## Compare and reports

* Compare: every analysis run in the session is kept with its data. Selected
  analyses appear side by side, with their removal paths on one plot and a
  note when they used different data.
* Export: a report as HTML, Word or PDF (Quarto; PDF through Typst, no LaTeX),
  plots as PNG or SVG, tables as XLSX or CSV, the R script and the stabilitest
  result as RDS.

## Guided mode and Learn

* A "What does this mean?" panel restates each result in plain language. Every
  sentence repeats a number stabilitest returned, and the score is read as
  fragile or robust only when stabilitest returned a label. Expert mode opens
  the results on the Code tab.
* Every measure in the score row has an info button with a one-sentence
  definition.
* Learn links the stabilitest repository and its case-study vignettes; About
  shows how to cite stabilitest.
* Accessibility: labelled controls, WCAG AA text contrast in both themes and
  plot colours at 3:1 or more.

## Deployment

* A Dockerfile (`rocker/r-ver` 4.6.1, Quarto 1.10.18, stabilitest v0.6.6)
  serving the app on port 3838 as a non-root user. The profile is chosen with
  `R_CONFIG_ACTIVE`. See `docs/deployment.md`.

## Fixes

* The public profile could not run: the default 1000 bootstrap resamples
  exceeded its limit of 500, and Guided mode hides that setting.
* Data with named rows (such as `mtcars`) lost the link between results and
  rows.

# stabilitest.viz 0.1.0

First release: the data layer and the analysis engine (milestones M0 to M2 of
`docs/PLAN.md`). Every number comes from stabilitest 0.6.5 or later; the app
computes no statistics of its own.

## Data

* Bundled examples (stabilitest data sets and synthetic CSVs) and uploads of
  CSV, TSV, Excel, SAS transport (`.xpt`) and `.sas7bdat` files. SAS variable
  labels are kept.
* ADaM detection (ADSL, BDS, ADTTE) with default filters, a
  one-row-per-subject check and a suggested mapping of columns to roles.

## Analysis

* Six methods: two groups (Welch, paired t, Wilcoxon, signed-rank,
  Brunner-Munzel), two proportions (Fisher, chi-square, proportions test),
  equivalence and non-inferiority (mean, risk difference, odds ratio), linear
  models / ANCOVA, generalised linear models (binomial, Poisson with offset)
  and Cox models.
* Guided steps: Data, Question, Map, Settings, Run. Map offers only columns
  that fit each role, and a run is refused with a plain message when the
  setup does not fit the method.
* Runs in background workers ('mirai') and can be cancelled.
* A "Use the validated Fisher settings" option for the one calibrated profile.

## Results

* Calibration banner: a categorical label only when stabilitest returns one;
  otherwise "No categorical label", numeric only.
* Composite score and its three components in equal boxes with neutral
  meters, the weights stabilitest used, and how the score combines them.
* Reproducible R code for every run, which gives the same result outside the
  app.

## Known limits

* The results explorer (removal path, influence, bootstrap, removed
  observations), Compare, reports and the Learn content come in later
  releases.
