# Development entry point: reload the package and run the app.
#   Rscript dev/run_dev.R            # default profile
#   Rscript dev/run_dev.R public     # public companion profile
args <- commandArgs(trailingOnly = TRUE)
profile <- if (length(args)) args[[1]] else NULL

pkgload::load_all(export_all = FALSE)
run_app(profile = profile)
