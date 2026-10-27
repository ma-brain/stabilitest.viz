# stabilitest.viz: the Shiny companion to the stabilitest R package.
#
#   docker build -t stabilitest-viz .
#   docker run --rm -p 3838:3838 stabilitest-viz                         # production profile
#   docker run --rm -p 3838:3838 -e R_CONFIG_ACTIVE=public stabilitest-viz
#
# Then open http://localhost:3838.

FROM rocker/r-ver:4.6.1

# Quarto renders the HTML, Word and PDF (Typst) reports; no LaTeX is needed
ARG QUARTO_VERSION=1.10.18
RUN apt-get update \
  && apt-get install -y --no-install-recommends ca-certificates curl \
  && arch="$(dpkg --print-architecture)" \
  && curl -fsSL -o /tmp/quarto.deb \
    "https://github.com/quarto-dev/quarto-cli/releases/download/v${QUARTO_VERSION}/quarto-${QUARTO_VERSION}-linux-${arch}.deb" \
  && apt-get install -y --no-install-recommends /tmp/quarto.deb \
  && rm /tmp/quarto.deb \
  && rm -rf /var/lib/apt/lists/*

RUN R -q -e 'install.packages("pak")'

# stabilitest at a fixed release, so an image always computes the same numbers
ARG STABILITEST_REF=v0.6.6
RUN R -q -e "pak::pkg_install('ma-brain/stabilitest@${STABILITEST_REF}')"

# Dependencies first, so code changes do not reinstall them
WORKDIR /build
COPY DESCRIPTION ./
RUN R -q -e 'pak::local_install_deps(upgrade = FALSE)'

COPY . .
RUN R -q -e 'pak::local_install(upgrade = FALSE, dependencies = FALSE)' \
  && rm -rf /build /tmp/* \
  && R -q -e 'pak::cache_clean()'

RUN useradd --create-home --shell /bin/bash app
USER app
WORKDIR /home/app

# Profiles are defined in inst/golem-config.yml: default, production, public
ENV R_CONFIG_ACTIVE=production
EXPOSE 3838

HEALTHCHECK --interval=30s --timeout=5s --start-period=30s \
  CMD curl -fs http://localhost:3838/ > /dev/null || exit 1

CMD ["R", "-q", "-e", "shiny::runApp(stabilitest.viz::run_app(), host = '0.0.0.0', port = 3838)"]
