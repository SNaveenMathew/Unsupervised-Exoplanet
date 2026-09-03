# Modernized Exoplanet Detection Pipeline with 1D-CNN Autoencoder & GPU Acceleration

library(readr)
library(dplyr)
library(RSQLite)
library(keras)
library(reshape2)
source("util.R")

#' Enable mixed-precision (fp16 compute / fp32 master weights) training when
#' a GPU is available.
#'
#' On a GPU with tensor cores (compute capability 7.0+ - any RTX/V100/
#' A-series/H-series card), this roughly doubles the achievable batch size
#' or model capacity for a fixed memory budget, at essentially no accuracy
#' cost for a model this size - directly relevant when training on a single
#' 16GB GPU. Skipped automatically on CPU-only setups, where fp16 ops
#' aren't hardware-accelerated and mixed precision provides no benefit (and
#' can even be slightly slower), and skipped gracefully if the installed
#' TF/Keras build doesn't support it. See build_conv1d_autoencoder()'s final
#' layer for the matching float32-output requirement this needs to stay
#' numerically stable.
#'
#' @return TRUE if mixed precision was enabled, FALSE otherwise (invisible)
enable_mixed_precision_if_available <- function() {
  has_gpu <- tryCatch({
    length(tensorflow::tf$config$list_physical_devices("GPU")) > 0
  }, error = function(e) FALSE)
  
  if (!has_gpu) {
    message("No GPU detected - training in default (fp32) precision.")
    return(invisible(FALSE))
  }
  
  ok <- tryCatch({
    tensorflow::tf$keras$mixed_precision$set_global_policy("mixed_float16")
    message("GPU detected - enabled mixed_float16 precision for training.")
    TRUE
  }, error = function(e) {
    message("Mixed precision not available (", conditionMessage(e), ") - continuing in default precision.")
    FALSE
  })
  invisible(ok)
}

#' Build a 1D Convolutional Autoencoder for light curve reconstruction
#'
#' @param seq_len Sequence window length (default: 128)
#' @param lr Learning rate for Adam optimizer (default: 0.001)
#' @return Compiled Keras model
build_conv1d_autoencoder <- function(seq_len = 128, lr = 0.001) {
  model <- keras_model_sequential() %>%
    # Encoder
    layer_conv_1d(filters = 32, kernel_size = 5, padding = "same", activation = "relu",
                  input_shape = c(seq_len, 1)) %>%
    layer_max_pooling_1d(pool_size = 2) %>%
    layer_conv_1d(filters = 64, kernel_size = 5, padding = "same", activation = "relu") %>%
    layer_max_pooling_1d(pool_size = 2) %>%
    layer_conv_1d(filters = 128, kernel_size = 3, padding = "same", activation = "relu") %>%
    layer_max_pooling_1d(pool_size = 2) %>%
    
    # Bottleneck representation
    layer_conv_1d(filters = 128, kernel_size = 3, padding = "same", activation = "relu") %>%
    
    # Decoder
    layer_upsampling_1d(size = 2) %>%
    layer_conv_1d(filters = 64, kernel_size = 5, padding = "same", activation = "relu") %>%
    layer_upsampling_1d(size = 2) %>%
    layer_conv_1d(filters = 32, kernel_size = 5, padding = "same", activation = "relu") %>%
    layer_upsampling_1d(size = 2) %>%
    # Forced to float32 even under a mixed_float16 global policy (see
    # enable_mixed_precision_if_available() above) - standard mixed-
    # precision practice: keeping the last layer's activation/loss
    # computation in float32 avoids numerical instability (e.g. NaN loss)
    # that a linear output layer combined with MSE loss can hit in fp16,
    # while every earlier layer still gets the fp16 speed/memory benefit.
    layer_conv_1d(filters = 1, kernel_size = 5, padding = "same", activation = "linear", dtype = "float32")
  
  model %>% compile(
    loss = "mse",
    optimizer = optimizer_adam(learning_rate = lr),
    metrics = c("mae")
  )
  
  return(model)
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
  
  enable_mixed_precision_if_available()
  
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
  
  # Callbacks for training
  callbacks_list <- list(
    callback_early_stopping(monitor = "val_loss", patience = 5, restore_best_weights = TRUE),
    callback_reduce_lr_on_plateau(monitor = "val_loss", factor = 0.5, patience = 2)
  )
  
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
      
      mdl_file <- file.path("trained_models", paste0(out_name, ".hdf5"))
      just_trained <- !file.exists(mdl_file)
      
      # 3. Model Training / Loading
      if(just_trained) {
        model <- build_conv1d_autoencoder(seq_len = seq_len)
        
        his <- model %>% fit(
          x = x_train, y = y_train,
          batch_size = min(batch_size, dim(x_train)[1]),
          epochs = epochs,
          validation_split = 0.2,
          callbacks = callbacks_list,
          verbose = 0
        )
        
        save_model_hdf5(model, mdl_file)
        
        # Save learning curve plot
        png(file.path("plots/learning_curve", paste0(out_name, "_learning.png")),
            width = 1366, height = 768)
        print(plot(his))
        dev.off()
        
        ep_actual <- length(his$metrics$loss)
        metrics_df <- rbind(metrics_df, data.frame(
          file = file,
          train_loss = his$metrics$loss[ep_actual],
          train_mae  = his$metrics$mae[ep_actual],
          val_loss   = his$metrics$val_loss[ep_actual],
          val_mae    = his$metrics$val_mae[ep_actual]
        ))
      } else {
        model <- load_model_hdf5(mdl_file)
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
      x_train_pred <- predict(model, x_train, verbose = 0)
      x_test_pred  <- predict(model, x_test, verbose = 0)
      
      train_plot_file <- file.path("plots/train_pred_plot", paste0(out_name, "_train_plot.png"))
      rec_train <- record_candidates(
        y_pred = x_train_pred, y = y_train, star_id = star_id,
        db_conn = mydb, table_name = "train_idx",
        plot_file = train_plot_file, force = just_trained
      )
      
      test_plot_file <- file.path("plots/test_pred_plot", paste0(out_name, "_test_plot.png"))
      rec_test <- record_candidates(
        y_pred = x_test_pred, y = y_test, star_id = star_id,
        db_conn = mydb, table_name = "test_idx",
        plot_file = test_plot_file, force = just_trained
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
