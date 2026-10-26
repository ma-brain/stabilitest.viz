# Upstream requests for stabilitest

Changes the app would benefit from. The app never edits `../stabilitest`.

## 1. `tidy()` / `as_tibble()` accessor (M2)
`robustness_analysis` returns a nested layout (`jackknife$results`, `extreme`) while
`robustness_model` is flat with `removed_rows`. A supported accessor returning one tidy
shape would replace most of the app's adapter layer and protect it from API drift.

## 2. Export the example datasets (M0, minor)
`pain_ancova_trial` and `onc_response_trial` are lazy data and work as
`stabilitest::pain_ancova_trial`, but only `pain_treatment` and `pain_placebo` are in
`NAMESPACE`. Listing all four in the documented exports would make the dependency explicit.

## 3. Character predictors in `robustness_glm()` and `robustness_surv()` (M2)
With a text (character) predictor, both fail with "Model could not be fitted on the full
dataset", while the same data with `factor(arm)` work. `robustness_lm()` accepts text columns.
The app converts text predictors to factors in its prepared data (shown in the generated
code), but a clear error or automatic conversion in stabilitest would help direct users.

## 4. Greedy removal set for `robustness_analysis()` (M2)
`robustness_model` results carry `removed_rows`, but `robustness_analysis` returns only the
removal path (`worstcase$results`) and `fragility_index`, not which observations were removed.
The M3 "Removed observations" table needs the set for the two-group tests too.

## 5. `removed_rows` indexing (M2, documentation)
`removed_rows` and jackknife `row` index the model frame after rows with missing values are
dropped, not the data frame passed in. The app drops incomplete rows itself and keeps the
row mapping, so it is unaffected, but a documented statement would avoid surprises.

## 6. `analysis_profile` for non-proportion tests (M2, minor)
For `t.test` on continuous data, `analysis_profile` contains proportion fields computed from
the continuous values (for example `events1 = -554`, `rate1 = -19.8`). They are harmless but
look like errors if shown. Returning `NULL` for them outside the proportion tests would be
clearer.

## 7. Bootstrap seed and the random number generator (M2)
`seed` gives the same numbers only under the same `RNGkind()`. Background workers can use
`L'Ecuyer-CMRG`, which changes every bootstrap result for the same seed. The app pins R's
default generator for runs and writes `RNGkind(...)` into the generated code. Recording
`RNGkind()` in the result, or setting it inside the functions, would remove the trap.
