# stabilitest.viz (development version)

* Compare (M4): every analysis run in the session is kept, with its data.
  Select analyses to see them side by side and their removal paths on one
  plot. A note appears when they were run on different data.
* Export tab (M4): a report as HTML, Word or PDF (rendered with Quarto; PDF
  through Typst, no LaTeX), plots as PNG or SVG, tables as XLSX or CSV, the R
  script and the stabilitest result as RDS.
* Results explorer (M3): tabs for the removal path, influence, bootstrap,
  removed observations, interpretation and code below the score. Plots are
  interactive and follow the light/dark switch. Selecting observations in the
  influence plot or the removed-observations table selects them in both.
* Each removal step links to the data row it removed, for every method.
* Fixed: data with named rows (such as `mtcars`) lost the link between results
  and rows.
* Requires stabilitest 0.6.6, which names the observations removed at each
  step of the two-group removal paths.

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
