# Modernized Exoplanet Detection Pipeline with 1D-CNN Autoencoder & GPU Acceleration

library(readr)
library(dplyr)
library(RSQLite)
library(keras)
library(reshape2)
source("util.R")

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
    layer_conv_1d(filters = 1, kernel_size = 5, padding = "same", activation = "linear")
  
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
  
  # Ensure output directories exist
  dir.create("plots/learning_curve", showWarnings = FALSE, recursive = TRUE)
  dir.create("plots/test_pred_plot", showWarnings = FALSE, recursive = TRUE)
  dir.create("plots/train_pred_plot", showWarnings = FALSE, recursive = TRUE)
  dir.create("trained_models", showWarnings = FALSE, recursive = TRUE)
  dir.create(dirname(db_file), showWarnings = FALSE, recursive = TRUE)
  
  # Initialize SQLite database and Kepler star metadata natively
  mydb <- dbConnect(RSQLite::SQLite(), db_file)
  
  files <- list.files(path = data_dir, pattern = "\\.tbl$", full.names = TRUE)
  if(length(files) == 0) {
    message("No .tbl files found in ", data_dir)
    dbDisconnect(mydb)
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
  train_idx_df <- data.frame()
  test_idx_df <- data.frame()
  
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
      
      # 3. Model Training / Loading
      if(!file.exists(mdl_file)) {
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
      
      # 4. Asymmetric Transit Candidate Detection on Train & Test sets
      # Predict reconstructions
      x_train_pred <- predict(model, x_train, verbose = 0)
      x_test_pred  <- predict(model, x_test, verbose = 0)
      
      # Evaluate candidate dips and save visual verification plots
      train_plot_file <- file.path("plots/train_pred_plot", paste0(out_name, "_train_plot.png"))
      temp_train_cands <- save_plot(
        y_pred = x_train_pred,
        y = y_train,
        out_file = train_plot_file
      )
      
      if(nrow(temp_train_cands) > 0) {
        temp_train_cands$id <- star_id
        temp_train_cands <- temp_train_cands[, c("id", "start", "end")]
      } else {
        temp_train_cands <- data.frame(id = star_id, start = 0, end = 0)
      }
      train_idx_df <- rbind(train_idx_df, temp_train_cands)
      
      test_plot_file <- file.path("plots/test_pred_plot", paste0(out_name, "_test_plot.png"))
      temp_test_cands <- save_plot(
        y_pred = x_test_pred,
        y = y_test,
        out_file = test_plot_file
      )
      
      if(nrow(temp_test_cands) > 0) {
        temp_test_cands$id <- star_id
        temp_test_cands <- temp_test_cands[, c("id", "start", "end")]
      } else {
        temp_test_cands <- data.frame(id = star_id, start = 0, end = 0)
      }
      test_idx_df <- rbind(test_idx_df, temp_test_cands)
      
    }, error = function(e) {
      err_msg <- sprintf("[%s] Error processing %s: %s\n", Sys.time(), files[i], as.character(e))
      cat(err_msg, file = "error_log.txt", append = TRUE)
      message("  ", err_msg)
    })
  }
  
  # 5. Update SQLite Database
  if(!("train_idx" %in% dbListTables(mydb))) {
    dbWriteTable(mydb, "train_idx", train_idx_df, overwrite = TRUE)
    dbWriteTable(mydb, "test_idx", test_idx_df, overwrite = TRUE)
  } else {
    insert_into_db(db_file, "train_idx", train_idx_df)
    insert_into_db(db_file, "test_idx", test_idx_df)
  }
  dbDisconnect(mydb)
  
  message(sprintf("Pipeline complete. Processed %d stars. Detected %d train candidates, %d test candidates.",
                  length(unique(c(train_idx_df$id, test_idx_df$id))),
                  nrow(train_idx_df[train_idx_df$start > 0, ]),
                  nrow(test_idx_df[test_idx_df$start > 0, ])))
  
  return(metrics_df)
}