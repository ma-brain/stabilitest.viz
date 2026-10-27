# Shared setup for the manuals. Every table that describes the app is read
# from the installed stabilitest.viz, so the manuals cannot drift from it.
stv <- asNamespace("stabilitest.viz")

md_table <- function(df, ...) knitr::kable(df, format = "pipe", row.names = FALSE, ...)

screenshot <- function(name, caption, width = "100%") {
  path <- file.path("images", paste0(name, ".png"))
  if (!file.exists(path)) {
    return(knitr::asis_output(paste0("*[Screenshot ", name, " not generated: run dev/screenshots.R]*\n\n")))
  }
  knitr::asis_output(sprintf("![%s](%s){width=%s}\n\n", caption, path, width))
}

profiles_table <- function() {
  cfg <- yaml::read_yaml(system.file("golem-config.yml", package = "stabilitest.viz"))
  get <- \(p, key) cfg[[p]][[key]] %||% cfg$default[[key]]
  profiles <- c("default", "production", "public")
  data.frame(
    Profile = paste0("`", profiles, "`"),
    Use = c("Local use", "Self-hosted server", "Public companion"),
    Uploads = vapply(profiles, \(p) if (get(p, "max_upload_mb") > 0) paste(get(p, "max_upload_mb"), "MB") else "Off", ""),
    `Max resamples` = vapply(profiles, \(p) format(get(p, "max_n_boot")), ""),
    Workers = vapply(profiles, \(p) format(get(p, "workers")), ""),
    Modes = c("Guided and Expert", "Guided and Expert", "Guided only, example preloaded"),
    check.names = FALSE
  )
}

`%||%` <- function(x, y) if (is.null(x)) y else x
