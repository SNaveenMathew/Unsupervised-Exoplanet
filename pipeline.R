# Modernized Exoplanet Detection Pipeline with 1D-CNN Autoencoder & GPU Acceleration
#
# Cross-platform: Keras/TensorFlow on Linux (native GPU support), PyTorch
# on Windows (TensorFlow dropped native Windows GPU support after 2.10;
# PyTorch maintains full current-CUDA support on Windows). Both backends
# are expected to live in the SAME-named Python environment (whatever
# main.R's reticulate::use_condaenv() points at) - only which package is
# installed inside it differs by OS. See util.R's "Cross-platform deep-
# learning backend" section for dl_backend()/dl_fit()/dl_predict()/
# dl_save()/dl_load() and everything else this file dispatches through;
# nothing below needs to know which backend is actually running.

library(readr)
library(dplyr)
library(RSQLite)
library(reshape2)
source("util.R")

# keras is only needed on the Linux/TensorFlow path. Loaded conditionally
# so a Windows machine with no keras R package installed at all (it only
# needs reticulate + Python's torch there) doesn't fail just from sourcing
# this file.
library(reticulate)
tryCatch({
  reticulate::use_condaenv("tf_gpu", required = FALSE)
}, error = function(e) {
  # Fall back to default python environment
})
if (identical(dl_backend(), "keras")) {
  library(keras)
}

#' Run the complete Unsupervised Exoplanet Detection Pipeline
#'
#' @param data_dir Path to folder containing Kepler .tbl light curves
#' @param seq_len Sequence length for sliding windows (default: 128 cadences ~ 2.6 days)
#' @param train_ratio Fraction of sequences used for training (default: 0.7)
#' @param batch_size Batch size for GPU training (default: 256)
#' @param epochs Maximum training epochs (default: 20)
#' @param run_hrs Maximum runtime in hours (default: 8)
#' @param db_file SQLite database path (default: "shiny/exoplanet_db.sqlite")
#' @return Dataframe of training/validation metrics
run_pipeline <- function(data_dir = "data", seq_len = 128, train_ratio = 0.7,
                         batch_size = 256, epochs = 20, run_hrs = 8,
                         db_file = "shiny/exoplanet_db.sqlite") {
  
  backend <- dl_backend()
  message("Deep-learning backend: ", backend,
          if (identical(backend, "torch")) " (PyTorch, Windows)" else " (Keras/TensorFlow, Linux)")
  use_amp <- dl_enable_mixed_precision(backend)
  
  # Ensure output directories exist
  dir.create("plots/learning_curve", showWarnings = FALSE, recursive = TRUE)
  dir.create("plots/test_pred_plot", showWarnings = FALSE, recursive = TRUE)
  dir.create("plots/train_pred_plot", showWarnings = FALSE, recursive = TRUE)
  dir.create("trained_models", showWarnings = FALSE, recursive = TRUE)
  dir.create(dirname(db_file), showWarnings = FALSE, recursive = TRUE)
  
  # Initialize SQLite database and Kepler star metadata natively
  mydb <- dbConnect(RSQLite::SQLite(), db_file)
  on.exit(dbDisconnect(mydb), add = TRUE)
  
  files <- list.files(path = data_dir, pattern = "\\.tbl$", full.names = TRUE)
  if(length(files) == 0) {
    message("No .tbl files found in ", data_dir)
    return(data.frame())
  }
  
  # Native cross-platform Kepler ID extraction and candidate count
  file_basenames <- basename(files)
  kplr_ids <- as.integer(gsub("^kplr([0-9]+)_.*", "\\1", file_basenames))
  
  if(!("kepler_star" %in% dbListTables(mydb))) {
    star_counts <- as.data.frame(table(kplr_ids))
    colnames(star_counts) <- c("id", "num_planets")
    star_counts$id <- as.integer(as.character(star_counts$id))
    dbWriteTable(mydb, "kepler_star", star_counts, overwrite = TRUE)
  }
  
  out_names <- tools::file_path_sans_ext(file_basenames)
  mdl_ext <- dl_model_ext(backend)
  
  tm_start <- Sys.time()
  metrics_df <- data.frame()
  n_stars_processed <- 0L
  n_train_cands <- 0L
  n_test_cands <- 0L
  
  message(sprintf("Starting pipeline on %d light curves with seq_len=%d, batch_size=%d...",
                  length(files), seq_len, batch_size))
  
  for(i in seq_along(files)) {
    diff_secs <- as.numeric(difftime(Sys.time(), tm_start, units = "secs"))
    if(diff_secs >= 3600 * run_hrs) {
      message("Maximum runtime reached. Stopping loop.")
      break
    }
    
    file <- files[i]
    star_id <- kplr_ids[i]
    out_name <- out_names[i]
    message(sprintf("[%d/%d] Processing %s (Kepler ID: %d)...", i, length(files), out_name, star_id))
    
    tryCatch({
      # 1. Read & Preprocess Kepler Light Curve
      raw_df <- read_kepler_table(file)
      cleaned_df <- clean_light_curve(raw_df)
      
      if(nrow(cleaned_df) < seq_len) {
        message("  Skipping: insufficient continuous cadences.")
        next
      }
      
      # 2. Extract Windows
      split_res <- split_train_test(cleaned_df, train_ratio = train_ratio, seq_len = seq_len)
      x_train <- split_res$x_train
      y_train <- split_res$y_train
      x_test  <- split_res$x_test
      y_test  <- split_res$y_test
      
      if(dim(x_train)[1] == 0) {
        message("  Skipping: zero training windows generated.")
        next
      }
      
      # dl_model_ext() picks .hdf5 (Keras) or .pt (PyTorch) to match the
      # active backend - a model trained under the OTHER backend during a
      # previous session on the other OS is simply not found here and
      # gets retrained fresh; the two formats can't load into each other.
      mdl_file <- file.path("trained_models", paste0(out_name, mdl_ext))
      just_trained <- !file.exists(mdl_file)
      
      # 3. Model Training / Loading - dl_fit()/dl_load() dispatch to
      # Keras or PyTorch (see util.R); both return/accept the same shapes
      # either way, so nothing below this point needs to know which ran.
      if(just_trained) {
        fit_result <- dl_fit(
          x_train = x_train, y_train = y_train, seq_len = seq_len,
          batch_size = batch_size, epochs = epochs, validation_split = 0.2,
          use_amp = use_amp, backend = backend
        )
        model <- fit_result$model
        his <- fit_result$history
        
        dl_save(model, mdl_file, backend = backend)
        
        # Save learning curve plot
        png(file.path("plots/learning_curve", paste0(out_name, "_learning.png")),
            width = 1366, height = 768)
        plot(his$loss, type = "l", col = "dodgerblue", lwd = 2,
             ylim = range(c(his$loss, his$val_loss), finite = TRUE),
             xlab = "Epoch", ylab = "Loss", main = paste("Training History -", out_name))
        lines(his$val_loss, col = "firebrick", lwd = 2, lty = 2)
        legend("topright", legend = c("Training loss", "Validation loss"),
               col = c("dodgerblue", "firebrick"), lty = c(1, 2), lwd = 2)
        dev.off()
        
        ep_actual <- length(his$loss)
        metrics_df <- rbind(metrics_df, data.frame(
          file = file,
          train_loss = his$loss[ep_actual],
          train_mae  = his$mae[ep_actual],
          val_loss   = his$val_loss[ep_actual],
          val_mae    = his$val_mae[ep_actual]
        ))
      } else {
        model <- dl_load(mdl_file, backend = backend)
      }
      
      # 4. Asymmetric Transit Candidate Detection on Train & Test sets,
      # recorded via record_candidates() (util.R) - the exact same function
      # score_star() (used by the Shiny app and precompute_all_stars.R)
      # calls for its own DB writes, so a star's candidates can never
      # silently differ depending on which part of the codebase computed
      # them. Writes happen immediately per star (rather than accumulated
      # in memory and written once at the very end, as before) so an
      # interrupted run - the run_hrs timeout above, or a crash - never
      # loses candidates for stars that were already successfully
      # processed. force = just_trained: a freshly retrained model
      # overwrites any stale candidates left over from a previous run; a
      # model merely reloaded from disk (nothing about it changed) respects
      # the existing cache and skips the redundant write - this also makes
      # re-running the whole pipeline over an already-processed data_dir
      # safe (no duplicate rows), which the original accumulate-then-
      # dbWriteTable/insert_into_db approach did not guarantee.
      x_train_pred <- dl_predict(model, x_train, backend = backend)
      x_test_pred  <- dl_predict(model, x_test, backend = backend)
      
      # merge_gap = seq_len - stride collapses redundant re-detections of
      # the same physical event across overlapping windows - see
      # detect_transit_candidates()'s merge_gap docs in util.R. stride here
      # matches split_train_test()'s own stride = max(1, floor(seq_len/4)).
      merge_gap <- seq_len - max(1, floor(seq_len / 4))
      
      train_plot_file <- file.path("plots/train_pred_plot", paste0(out_name, "_train_plot.png"))
      rec_train <- record_candidates(
        y_pred = x_train_pred, y = y_train, star_id = star_id,
        db_conn = mydb, table_name = "train_idx",
        plot_file = train_plot_file, force = just_trained, merge_gap = merge_gap
      )
      
      test_plot_file <- file.path("plots/test_pred_plot", paste0(out_name, "_test_plot.png"))
      rec_test <- record_candidates(
        y_pred = x_test_pred, y = y_test, star_id = star_id,
        db_conn = mydb, table_name = "test_idx",
        plot_file = test_plot_file, force = just_trained, merge_gap = merge_gap
      )
      
      n_stars_processed <- n_stars_processed + 1L
      n_train_cands <- n_train_cands + nrow(rec_train$candidates)
      n_test_cands  <- n_test_cands + nrow(rec_test$candidates)
      
    }, error = function(e) {
      err_msg <- sprintf("[%s] Error processing %s: %s\n", Sys.time(), files[i], as.character(e))
      cat(err_msg, file = "error_log.txt", append = TRUE)
      message("  ", err_msg)
    })
  }
  
  message(sprintf(
    "Pipeline complete. Processed %d star(s). Detected %d train candidate window(s), %d test candidate window(s).",
    n_stars_processed, n_train_cands, n_test_cands
  ))
  
  return(metrics_df)
}
