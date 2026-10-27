# Deployment

stabilitest.viz runs anywhere R runs, as a package (`run_app()`) or as a
Docker image. It keeps no state between sessions and writes nothing outside
temporary folders.

## As an R package

```r
pak::pak("ma-brain/stabilitest.viz")
shiny::runApp(stabilitest.viz::run_app(profile = "production"), host = "0.0.0.0", port = 3838)
```

Reports need the Quarto CLI on the `PATH` (`quarto::quarto_path()` must find
it). PDF reports use Typst, which ships with Quarto, so no LaTeX is needed.

## Docker image

The `Dockerfile` builds on `rocker/r-ver:4.6.1` and adds:

- Quarto `1.10.18` (build argument `QUARTO_VERSION`), for the reports;
- stabilitest at tag `v0.6.6` (build argument `STABILITEST_REF`), so an image
  always computes the same numbers;
- the app and its dependencies, installed with `pak` (which also installs the
  system libraries they need).

The app runs as the non-root user `app` on port 3838, with a health check on
`/`.

```sh
docker build -t stabilitest-viz .
docker build -t stabilitest-viz --build-arg STABILITEST_REF=v0.6.6 .
docker run --rm -p 3838:3838 stabilitest-viz
```

### Profiles

Set `R_CONFIG_ACTIVE` to choose a profile from `inst/golem-config.yml`. The
image defaults to `production`.

| Profile      | Uploads | Max resamples | Workers | Mode                    |
|--------------|---------|---------------|---------|-------------------------|
| `default`    | 50 MB   | 5000          | 2       | Guided and Expert       |
| `production` | 25 MB   | 2000          | 4       | Guided and Expert       |
| `public`     | off     | 500           | 2       | Guided only, example preloaded |

```sh
docker run --rm -p 3838:3838 -e R_CONFIG_ACTIVE=public stabilitest-viz
```

### Resources

Each Shiny process starts its own `mirai` background workers (`workers` in the
profile) for analyses, so a container uses one R process plus that many
workers. A few hundred MB of memory per container is enough for the bundled
examples; large uploads and many resamples need more. Run several containers
behind a load balancer with sticky sessions to serve more users.

## Continuous integration

`.github/workflows/docker.yaml` builds the image on every pull request and
push to `main`, starts it with the public profile, checks that the page is
served, and runs an analysis and renders the three report formats inside the
container. The image is not pushed to a registry.
