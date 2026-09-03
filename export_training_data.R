#!/usr/bin/env Rscript
# export_training_data.R
#
# Closes the loop between crowd tagging and model retraining: exports two
# CSVs from the app's existing user_star/test_idx tagging data, ready for
# an offline retraining/fine-tuning script (pipeline.R / main.R, not part
# of this Shiny app) to pick up:
#
#   training_labels_consensus.csv - regions where every human tagger for a
#     star unanimously agrees (>= --min-agreement of them) - trustworthy
#     positive labels.
#   training_labels_disputed.csv  - regions where SOME but not all human
#     taggers for a star flagged something - genuine disagreement, and
#     exactly the kind of hard example worth prioritizing for fine-tuning
#     (the same regions the in-app bandit selector already surfaces to
#     human taggers as needing another look).
#
# Usage:
#   Rscript export_training_data.R
#   Rscript export_training_data.R --min-agreement=3 --out-dir=exports/
#   Rscript export_training_data.R --db=custom/path.sqlite
#
# Lives at the REPO ROOT, alongside main.R/pipeline.R/util.R - run it from
# there, the same place you'd run `Rscript main.R`:
#
#   Unsupervised-Exoplanet/
#   |-- main.R
#   |-- pipeline.R
#   |-- util.R
#   |-- precompute_all_stars.R
#   |-- export_training_data.R   <- this file
#   |-- data/*.tbl
#   |-- trained_models/*.hdf5
#   `-- shiny/
#       |-- app.Rmd              <- read for its shared helper functions
#       `-- exoplanet_db.sqlite  <- default DB location (matches main.R's --db default)
#
# Reuses export_training_labels() (and the functions it depends on -
# build_community_regions, get_star_human_tag_sets, compute_star_bandit_
# stats, etc.) directly from app.Rmd's first R chunk, rather than a
# hand-copied duplicate that could drift out of sync with the live app.
#
# IMPLEMENTATION NOTE: app.Rmd's global chunk is more than function
# definitions - it also does its own library() calls, opens its own db
# connection, and resolves its own paths via `<<-` (superassignment),
# which - if that whole chunk were eval()'d as-is - would (a) require every
# shiny/shinydashboard/etc. package the app itself needs just to read a
# handful of pure helper functions, (b) silently create a stray, empty
# exoplanet_db.sqlite wherever this script happens to be run from (its
# path resolution assumes it's being run from inside shiny/, which Shiny
# itself guarantees but this standalone script does not), and (c) - because
# `<<-` walks up parent environments and, when nothing existing is found,
# falls back to creating the binding in the GLOBAL environment regardless
# of which environment eval() targets - can silently clobber this script's
# own same-named variables. To avoid all three, only the top-level
# `name <- function(...) { ... }` definitions are extracted and evaluated
# below; every other top-level statement in app.Rmd's chunk (library()
# calls, its own db connection, its own path resolution) is skipped
# entirely.

suppressPackageStartupMessages({
  library(readr); library(dplyr); library(RSQLite)
})

args <- commandArgs(trailingOnly = TRUE)
min_agreement <- {
  m <- grep("^--min-agreement=", args, value = TRUE)
  if (length(m) > 0) as.integer(sub("^--min-agreement=", "", m[1])) else 2
}
out_dir <- {
  m <- grep("^--out-dir=", args, value = TRUE)
  if (length(m) > 0) sub("^--out-dir=", "", m[1]) else "."
}
export_db_override <- {
  m <- grep("^--db=", args, value = TRUE)
  if (length(m) > 0) sub("^--db=", "", m[1]) else NULL
}

if (!dir.exists("data")) {
  stop("Could not find data/ in the current directory. ",
       "Run this script from the repo root (the same place you'd run `Rscript main.R`).")
}
export_data_dir <- "data/"

# app.Rmd's real, established location is shiny/app.Rmd - fall back to a
# flat copy only for the unusual case of running this from inside a
# flattened (e.g. shinyapps.io-style) deployment bundle.
export_app_rmd_path <- if (file.exists("shiny/app.Rmd")) {
  "shiny/app.Rmd"
} else if (file.exists("app.Rmd")) {
  "app.Rmd"
} else {
  stop("Could not find app.Rmd (looked for 'shiny/app.Rmd' and 'app.Rmd'). ",
       "Run this script from the repo root (the same place you'd run `Rscript main.R`).")
}

# Matches main.R's own --db default (shiny/exoplanet_db.sqlite), falling
# back to a root-level copy if that's what's actually present (see the
# README's own note that both locations have been used historically).
export_sql_db_file <- if (!is.null(export_db_override)) {
  export_db_override
} else if (file.exists("shiny/exoplanet_db.sqlite")) {
  "shiny/exoplanet_db.sqlite"
} else {
  "exoplanet_db.sqlite"
}

# Pulls out ONLY top-level `name <- function(...) { ... }` definitions from
# app.Rmd's first R chunk - see the implementation note above for why the
# rest of that chunk is deliberately never evaluated.
export_extract_helper_functions <- function(rmd_path, envir) {
  lines <- readLines(rmd_path, warn = FALSE)
  starts <- grep("^```\\{r", lines)
  ends <- grep("^```\\s*$", lines)
  if (length(starts) == 0) stop("No R chunks found in ", rmd_path)
  first_end <- ends[ends > starts[1]][1]
  chunk_lines <- lines[(starts[1] + 1):(first_end - 1)]
  
  exprs <- parse(text = chunk_lines)
  n_defs <- 0L
  for (e in exprs) {
    is_fn_def <- is.call(e) && identical(as.character(e[[1]]), "<-") &&
      length(e) == 3 && is.call(e[[3]]) && identical(as.character(e[[3]][[1]]), "function")
    if (is_fn_def) {
      eval(e, envir = envir)
      n_defs <- n_defs + 1L
    }
  }
  n_defs
}

export_helper_env <- new.env(parent = globalenv())
n_defs <- export_extract_helper_functions(export_app_rmd_path, export_helper_env)
if (!exists("export_training_labels", envir = export_helper_env, inherits = FALSE)) {
  stop("export_training_labels() was not found among the ", n_defs,
       " function definition(s) extracted from ", export_app_rmd_path,
       " - has it been renamed or moved out of the first R chunk?")
}

cat(sprintf("Using database: %s\n", export_sql_db_file))
export_db_conn <- dbConnect(RSQLite::SQLite(), export_sql_db_file)
on.exit(dbDisconnect(export_db_conn), add = TRUE)

export_star_files <- list.files(export_data_dir, pattern = "\\.tbl$")
export_star_options <- unique(vapply(strsplit(tools::file_path_sans_ext(export_star_files), "_"), `[`, character(1), 1))

cat(sprintf("Exporting training labels for %d star(s), min-agreement=%d...\n", length(export_star_options), min_agreement))

export_result <- export_helper_env$export_training_labels(export_db_conn, export_star_options, min_agreement = min_agreement)

dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
export_consensus_path <- file.path(out_dir, "training_labels_consensus.csv")
export_disputed_path  <- file.path(out_dir, "training_labels_disputed.csv")
readr::write_csv(export_result$consensus, export_consensus_path)
readr::write_csv(export_result$disputed, export_disputed_path)

cat(sprintf(
  "Wrote %d consensus row(s) to %s\nWrote %d disputed row(s) to %s\n",
  nrow(export_result$consensus), export_consensus_path, nrow(export_result$disputed), export_disputed_path
))
