# Containerised CP2021 level-4 imputation for OJA postings.
#
# Reads from the OJA Postgres, imputes the missing CP codes with
# skillviz::predict_cp4_knn(), writes to the staging schema.
#
# Credentials are NEVER baked in: pass them at runtime with --env-file, or
# bind-mount an .Renviron. See postgres.env.example.
#
# Build from the package root, not from this directory:
#   docker build -f container/Dockerfile -t skillviz-impute:latest .

FROM rocker/r-ver:4.5.2

# 1. Environment --------------------------------------------------------------
# The CRAN snapshot is pinned so rebuilds resolve the same package versions.
ENV DEBIAN_FRONTEND=noninteractive \
    LANG=C.UTF-8 \
    LC_ALL=C.UTF-8 \
    TZ=Europe/Rome \
    RENV_CONFIG_REPOS_OVERRIDE=https://packagemanager.posit.co/cran/__linux__/noble/2026-01-15

# 2. System dependencies ------------------------------------------------------
# libpq5: RPostgres client library. procps: `ps`, used by the lock file for
# stale-PID detection. git: remotes needs it to resolve the Remotes: field.
# libglpk40 / libxml2 / libgmp10: igraph's binary links against all three and
# fails to LOAD without them -- skillviz Imports igraph, so the package cannot
# be attached at all. The build's load-check catches this rather than cron.
RUN apt-get update && apt-get install -y --no-install-recommends \
      ca-certificates \
      curl \
      git \
      libpq5 \
      libglpk40 \
      libxml2 \
      libgmp10 \
      build-essential \
      locales \
      procps \
      tzdata \
    && rm -rf /var/lib/apt/lists/*

# 3. CRAN dependencies --------------------------------------------------------
# Installed and loaded in the same layer, so a package that installs but fails
# to load breaks the build here rather than at 03:00 in cron.
RUN Rscript -e 'options(repos = c(CRAN = Sys.getenv("RENV_CONFIG_REPOS_OVERRIDE"))); \
                options(Ncpus = parallel::detectCores()); \
                install.packages(c("DBI","RPostgres","data.table","Matrix","igraph","collapse","fst","remotes","xgboost")); \
                invisible(lapply(c("DBI","RPostgres","data.table","Matrix","igraph","collapse","fst","xgboost"), library, character.only = TRUE))' \
    && rm -rf /tmp/Rtmp* /tmp/downloaded_packages /root/.cache

# 4. skillviz -----------------------------------------------------------------
# install_local() honours the Remotes: field in DESCRIPTION, which pulls
# itaposts (and duckdb) from GitHub. predict_cp4_knn() does not use itaposts at
# runtime -- the only live call is in read_oja_itaposts() -- so moving itaposts
# to Suggests behind check_suggests() would drop duckdb, dplyr, dbplyr and
# processx from this image and remove the need for network access to GitHub.
# Left as a follow-up so this container introduces no package change.
WORKDIR /build
COPY . /build
RUN Rscript -e 'options(repos = c(CRAN = Sys.getenv("RENV_CONFIG_REPOS_OVERRIDE"))); \
                remotes::install_local("/build", dependencies = TRUE, upgrade = "never")' \
    && Rscript -e 'stopifnot(requireNamespace("skillviz", quietly = TRUE))' \
    && rm -rf /build /tmp/Rtmp* /tmp/downloaded_packages /root/.cache

# 5. Runtime layout -----------------------------------------------------------
# Fixed paths, bind-mounted by the caller. Keep these OUT of the env file.
ENV IMPUTE_LOCK_DIR=/var/skillviz/run
RUN mkdir -p /var/skillviz/run /app

COPY container/impute_cp4.R /app/impute_cp4.R

# 6. Non-root ------------------------------------------------------------------
# uid/gid 1000 so bind-mounted state stays writable when the caller passes
# --user "$(id -u):$(id -g)".
RUN groupadd --system --gid 1000 app \
    && useradd --system --uid 1000 --gid app --home /app app \
    && chown -R app:app /app /var/skillviz
USER app
WORKDIR /app

# 7. Entry point ---------------------------------------------------------------
# Split so `docker run IMAGE -e '<expr>'` still works for diagnostics.
ENTRYPOINT ["Rscript", "--no-save", "--no-restore", "--no-init-file", "--no-site-file"]
CMD ["/app/impute_cp4.R"]
