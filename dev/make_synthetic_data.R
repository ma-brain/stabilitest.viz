# Generates synthetic CSV files for trying the app and for M1 upload/ADaM tests.
# All data are simulated; none are real. Re-run to regenerate (fixed seed).
#   Rscript dev/make_synthetic_data.R
set.seed(20261026)
out <- file.path("inst", "extdata", "synthetic")
dir.create(out, recursive = TRUE, showWarnings = FALSE)
write <- function(x, name) {
  utils::write.csv(x, file.path(out, name), row.names = FALSE, na = "")
}

# 1. Two-sample continuous (Welch / Wilcoxon / Brunner-Munzel / TOST mean)
n <- 60
two_sample <- data.frame(
  subject_id = sprintf("S%03d", seq_len(2 * n)),
  arm = rep(c("Active", "Control"), each = n),
  outcome = c(rnorm(n, 52, 10), rnorm(n, 47, 12))
)
two_sample$outcome[c(7, 95)] <- c(88, 8) # two outliers
write(two_sample, "two_sample_continuous.csv")

# 2. Paired (paired t / signed-rank / paired TOST)
m <- 40
before <- rnorm(m, 140, 15)
paired <- data.frame(
  subject_id = sprintf("P%03d", seq_len(m)),
  before = round(before, 1),
  after = round(before - rnorm(m, 6, 8), 1)
)
write(paired, "paired_bp.csv")

# 3. Binary response (Fisher / chi-square / prop / TOST prop and OR)
b <- 70
binary <- data.frame(
  subject_id = sprintf("B%03d", seq_len(2 * b)),
  arm = rep(c("Active", "Control"), each = b),
  responder = c(rbinom(b, 1, 0.42), rbinom(b, 1, 0.22))
)
write(binary, "binary_response.csv")

# 4. ANCOVA (linear model), with a few missing outcomes
a <- 90
ancova <- data.frame(
  subject_id = sprintf("A%03d", seq_len(a)),
  arm = factor(sample(c("Placebo", "Low", "High"), a, replace = TRUE),
               levels = c("Placebo", "Low", "High")),
  age = round(rnorm(a, 58, 9)),
  baseline = round(rnorm(a, 60, 10), 1)
)
dose <- c(Placebo = 0, Low = -3, High = -6)[as.character(ancova$arm)]
ancova$change <- round(-2 + dose + 0.1 * (ancova$baseline - 60) + rnorm(a, 0, 8), 1)
ancova$change[sample(a, 4)] <- NA
write(ancova, "ancova_trial.csv")

# 5. Count outcome with exposure (Poisson GLM with offset)
k <- 100
counts <- data.frame(
  subject_id = sprintf("C%03d", seq_len(k)),
  arm = rep(c("Active", "Control"), each = k / 2),
  exposure_years = round(runif(k, 0.5, 2), 2)
)
counts$events <- rpois(k, exp(0.1 + ifelse(counts$arm == "Active", -0.35, 0)) * counts$exposure_years)
write(counts, "count_events.csv")

# 6. Time to event (Cox PH); event = 1 means event observed
t <- 120
tte <- data.frame(
  subject_id = sprintf("T%03d", seq_len(t)),
  arm = rep(c("Active", "Control"), each = t / 2),
  age = round(rnorm(t, 62, 8))
)
latent <- rexp(t, rate = 0.05 * exp(ifelse(tte$arm == "Active", -0.5, 0) + 0.02 * (tte$age - 62)))
censor <- runif(t, 6, 36)
tte$time_months <- round(pmin(latent, censor), 1)
tte$event <- as.integer(latent <= censor)
write(tte, "survival_cox.csv")

# 7-9. ADaM-style trio (for M1 ADaM detection): ADSL, ADEFF (BDS), ADTTE
s <- 100
adsl <- data.frame(
  STUDYID = "SYN-001",
  USUBJID = sprintf("SYN-001-%03d", seq_len(s)),
  TRT01P = rep(c("Placebo", "Drug X"), each = s / 2),
  AGE = round(rnorm(s, 55, 11)),
  SEX = sample(c("F", "M"), s, replace = TRUE),
  SAFFL = "Y",
  ITTFL = c(rep("Y", s - 3), "N", "N", "N"),
  EFFFL = c(rep("Y", s - 6), rep("N", 6))
)
write(adsl, "adsl.csv")

visits <- c("Baseline", "Week 4", "Week 12")
base <- rnorm(s, 50, 8)
effect <- ifelse(adsl$TRT01P == "Drug X", -5, -1)
adeff <- do.call(rbind, lapply(seq_along(visits), \(v) {
  aval <- base + c(0, 0.4, 1)[v] * effect + rnorm(s, 0, 6)
  if (v == 1) aval <- base
  data.frame(
    STUDYID = "SYN-001",
    USUBJID = adsl$USUBJID,
    TRTP = adsl$TRT01P,
    PARAMCD = "PAIN",
    PARAM = "Pain score (0-100)",
    AVISIT = visits[v],
    AVISITN = c(0, 4, 12)[v],
    AVAL = round(aval, 1),
    BASE = round(base, 1),
    CHG = round(aval - base, 1),
    ANL01FL = "Y",
    ITTFL = adsl$ITTFL
  )
}))
write(adeff, "adeff.csv")

adtte <- data.frame(
  STUDYID = "SYN-001",
  USUBJID = adsl$USUBJID,
  TRT01P = adsl$TRT01P,
  PARAMCD = "OS",
  PARAM = "Overall survival (months)"
)
latent <- rexp(s, rate = 0.06 * exp(ifelse(adsl$TRT01P == "Drug X", -0.45, 0)))
censor <- runif(s, 8, 30)
adtte$AVAL <- round(pmin(latent, censor), 1)
adtte$CNSR <- as.integer(latent > censor) # 1 = censored (ADaM convention)
adtte$ITTFL <- adsl$ITTFL
write(adtte, "adtte.csv")

message("Wrote ", length(list.files(out)), " files to ", out)
