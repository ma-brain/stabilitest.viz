# stabilitest.viz <a href="https://github.com/ma-brain/stabilitest.viz"><img src="man/figures/logo.png" align="right" height="139" alt="stabilitest.viz hex logo" /></a>

Interactive companion app for [stabilitest](https://github.com/ma-brain/stabilitest):
how easily could a statistical conclusion be overturned?

The app never re-implements statistics. Every number comes from stabilitest's
exported functions, and a categorical label (for example Fragile) is shown only
when stabilitest returns one. Otherwise the app says the result is numeric only.

> Status: **v1.0.0.** Load data, choose one of six methods, map columns, run in
> the background, read the score and explore it (removal path, influence,
> bootstrap, removed observations, interpretation, code), compare analyses and
> export a report, with a plain-language Guided mode. See [NEWS.md](NEWS.md).

## Installation

```r
# install.packages("pak")
pak::pak("ma-brain/stabilitest.viz")
```

## Usage

```r
library(stabilitest.viz)

run_app()                     # local use
run_app(profile = "public")   # public companion: Guided mode, no uploads
run_app(profile = "production")
```

`run_app()` returns a Shiny app object; printing it (as above at the console)
runs it. Pass Shiny options through, for example
`shiny::runApp(run_app(), port = 3838, launch.browser = TRUE)`.

| Profile      | Use                    | Uploads | Max resamples |
|--------------|------------------------|---------|---------------|
| `default`    | local use              | 50 MB   | 5000          |
| `production` | self-hosted server     | 25 MB   | 2000          |
| `public`     | public companion       | off     | 500           |

Profiles are defined in `inst/golem-config.yml`. Without `profile`, the
`R_CONFIG_ACTIVE` environment variable is used if it names one of them.
Reports need the [Quarto](https://quarto.org) command-line tool; without it the
Export tab offers everything except the report.

## Docker

```sh
docker build -t stabilitest-viz .
docker run --rm -p 3838:3838 stabilitest-viz                          # production
docker run --rm -p 3838:3838 -e R_CONFIG_ACTIVE=public stabilitest-viz
```

Then open <http://localhost:3838>. The image includes Quarto, so reports work.
See [docs/deployment.md](docs/deployment.md).

## Development

```r
Rscript dev/run_dev.R          # reload and run
devtools::document(); devtools::test(); devtools::check()
```

See [CLAUDE.md](CLAUDE.md) for project rules and [docs/](docs/) for the plan and brand.

## Licence

MIT, Marius Ardelean.
