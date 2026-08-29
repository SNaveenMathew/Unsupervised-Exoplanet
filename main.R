# Main entry point for Unsupervised Exoplanet Detection

library(readr)
library(reticulate)
# Attempt to use tf_gpu conda env if present
tryCatch({
  reticulate::use_condaenv("tf_gpu", required = FALSE)
}, error = function(e) {
  # Fall back to default python/keras environment
})

library(keras)
library(reshape2)
library(dplyr)
library(docopt)
library(RSQLite)
source("util.R")
source("pipeline.R")

doc <- "Usage: main.R [--PATH=<data_dir>] [--seq_len=<len>] [--batch_size=<bs>] [--epochs=<ep>] [--run_hrs=<hrs>] [--db=<db_file>]

Options:
  --PATH=<data_dir>      Relative path of folder containing light curves [default: data]
  --seq_len=<len>        Sequence window length in cadences [default: 128]
  --batch_size=<bs>      Batch size for GPU training [default: 256]
  --epochs=<ep>          Training epochs per star/batch [default: 20]
  --run_hrs=<hrs>        Maximum pipeline runtime in hours [default: 8]
  --db=<db_file>         SQLite database output path [default: shiny/exoplanet_db.sqlite]
"

opt <- docopt(doc)

set.seed(42)

run_pipeline(
  data_dir   = opt$PATH,
  seq_len    = as.integer(opt$seq_len),
  batch_size = as.integer(opt$batch_size),
  epochs     = as.integer(opt$epochs),
  run_hrs    = as.numeric(opt$run_hrs),
  db_file    = opt$db
)
