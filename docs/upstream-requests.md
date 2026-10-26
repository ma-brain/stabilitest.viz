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
