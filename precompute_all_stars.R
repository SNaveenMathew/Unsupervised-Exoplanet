#!/usr/bin/env Rscript
# precompute_all_stars.R
#
# Batch-scores every star under data/ using score_star() (see util.R),
# writing results into the shared exoplanet_db.sqlite test_idx cache table.
# Run this offline (cron, a nightly scheduled job, a GPU box with spare
# capacity, etc.) so the Shiny app's on-demand view is always reading from
# a warm cache instead of paying model-inference latency inside a live
# user session - this is the "a 16GB GPU can process thousands of stars
# overnight, decoupling GPU latency from human-facing latency" idea,
# literally implemented and runnable as:
#
#   Rscript precompute_all_stars.R
#   Rscript precompute_all_stars.R --force              # recompute everything
#   Rscript precompute_all_stars.R --limit=50            # just the first 50 (dry run)
#   Rscript precompute_all_stars.R --db=custom/path.sqlite
#
# Lives at the REPO ROOT, alongside main.R/pipeline.R/util.R (same folder
# that already holds data/ and trained_models/) - run it from there, the
# same place you'd run `Rscript main.R`:
#
#   Unsupervised-Exoplanet/
#   |-- main.R
#   |-- pipeline.R
#   |-- util.R
#   |-- precompute_all_stars.R   <- this file
#   |-- export_training_data.R
#   |-- data/*.tbl
#   |-- trained_models/*.hdf5
#   `-- shiny/
#       |-- app.Rmd
#       `-- exoplanet_db.sqlite  <- default DB location (matches main.R's --db default)
#
# Uses score_star() from util.R - the exact same function output$trainPlot
# in app.Rmd calls - so a star scored here and a star scored live in the
# app can never silently disagree with each other.

suppressPackageStartupMessages({
  library(readr); library(dplyr); library(RSQLite)
})

args <- commandArgs(trailingOnly = TRUE)
force <- "--force" %in% args
limit <- {
  m <- grep("^--limit=", args, value = TRUE)
  if (length(m) > 0) as.integer(sub("^--limit=", "", m[1])) else Inf
}
db_override <- {
  m <- grep("^--db=", args, value = TRUE)
  if (length(m) > 0) sub("^--db=", "", m[1]) else NULL
}

if (!file.exists("util.R") || !dir.exists("data")) {
  stop("Could not find util.R and data/ in the current directory. ",
       "Run this script from the repo root (the same place you'd run `Rscript main.R`).")
}
source("util.R")

data_dir   <- "data/"
models_dir <- "trained_models/"

# Matches main.R's own --db default (shiny/exoplanet_db.sqlite) - that's
# the real, established location pipeline.R/main.R write to - falling back
# to a root-level copy if that's what's actually present (see the README's
# own note that both locations have been used historically).
sql_db_file <- if (!is.null(db_override)) {
  db_override
} else if (file.exists("shiny/exoplanet_db.sqlite")) {
  "shiny/exoplanet_db.sqlite"
} else {
  "exoplanet_db.sqlite"
}

all_tbl <- list.files(data_dir, pattern = "\\.tbl$", full.names = TRUE)
if (is.finite(limit)) all_tbl <- head(all_tbl, limit)

cat(sprintf("Found %d star(s) under %s\n", length(all_tbl), data_dir))
cat(sprintf("Using database: %s\n", sql_db_file))
if (length(all_tbl) == 0) quit(save = "no", status = 0)

db_conn <- dbConnect(RSQLite::SQLite(), sql_db_file)
on.exit(dbDisconnect(db_conn), add = TRUE)

t0 <- Sys.time()
n_scored <- 0L; n_cached <- 0L; n_triage_skip <- 0L; n_errors <- 0L

for (f in all_tbl) {
  out_base <- tools::file_path_sans_ext(basename(f))
  mdl_candidates <- c(
    file.path(models_dir, paste0(out_base, ".hdf5")),
    file.path(models_dir, "global_conv1d_autoencoder.hdf5")
  )
  
  res <- tryCatch(
    score_star(tbl_file = f, trained_model_paths = mdl_candidates,
               db_conn = db_conn, plot_file = NULL, force = force),
    error = function(e) { message("  ERROR scoring ", out_base, ": ", conditionMessage(e)); NULL }
  )
  
  if (is.null(res)) { n_errors <- n_errors + 1L; next }
  
  if (isTRUE(res$from_cache)) {
    n_cached <- n_cached + 1L
    cat(sprintf("  [cached]  %s\n", out_base))
  } else {
    n_scored <- n_scored + 1L
    if (identical(res$fallback_reason, "triage_skip")) n_triage_skip <- n_triage_skip + 1L
    cat(sprintf("  [scored]  %-40s fallback=%-12s candidates=%d\n",
                out_base, if (is.null(res$fallback_reason)) "none" else res$fallback_reason, nrow(res$candidates)))
  }
}

elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
cat(sprintf(
  "\nDone in %.1fs: %d newly scored (%d via triage skip, compute saved), %d already cached, %d error(s), %d total.\n",
  elapsed, n_scored, n_triage_skip, n_cached, n_errors, length(all_tbl)
))
