# Utility functions for Kepler Light Curve Preprocessing and Candidate Detection

library(readr)
library(dplyr)
library(RSQLite)

#' Read Kepler Data Validation (.tbl) files
#'
#' @param file Path to .tbl file
#' @return data.frame with TIME, CADENCENO, RESIDUAL_FLUX, and original line index
read_kepler_table <- function(file) {
  lines <- read_lines(file, n_max = 50)
  col_line_idx <- grep("^\\s*\\|", lines)
  
  if(length(col_line_idx) > 0) {
    header_line <- lines[col_line_idx[1]]
    cols <- strsplit(header_line, "\\|")[[1]]
    cols <- trimws(cols)
    cols <- cols[cols != ""]
    
    # Read table data starting after header and separator lines
    data_start <- col_line_idx[length(col_line_idx)] + 1
    # Check if next line is a separator (e.g., dashes)
    if(grepl("^\\s*[-|\\s]+\\s*$", lines[data_start])) {
      data_start <- data_start + 1
    }
  } else {
    cols <- NULL
    data_start <- 35
  }
  
  df <- suppressWarnings(
    read_table(file, skip = data_start - 1, col_names = FALSE, show_col_types = FALSE)
  )
  
  if(!is.null(cols) && ncol(df) == length(cols)) {
    colnames(df) <- cols
  } else if(!is.null(cols) && ncol(df) >= 7) {
    # Fallback to standard Kepler DV table column mapping
    colnames(df)[1:min(ncol(df), length(cols))] <- cols[1:min(ncol(df), length(cols))]
  }
  
  # Ensure standard column names exist
  col_names_upper <- toupper(colnames(df))
  time_col <- grep("TIME", col_names_upper, value = TRUE)[1]
  flux_col <- grep("RESIDUAL_FLUX|FLUX", col_names_upper, value = TRUE)[1]
  cad_col  <- grep("CADENCENO|CADENCE", col_names_upper, value = TRUE)[1]
  
  if(is.na(flux_col)) flux_col <- colnames(df)[min(3, ncol(df))]
  if(is.na(time_col)) time_col <- colnames(df)[min(2, ncol(df))]
  if(is.na(cad_col))  cad_col  <- colnames(df)[1]
  
  res_df <- data.frame(
    cadence = as.numeric(df[[cad_col]]),
    time = as.numeric(df[[time_col]]),
    flux = as.numeric(df[[flux_col]]),
    orig_idx = seq_len(nrow(df))
  )
  
  # Filter out non-finite entries
  res_df <- res_df[is.finite(res_df$time) & is.finite(res_df$flux), ]
  return(res_df)
}

#' Preprocess light curve: Split at gaps, detrend stellar variability, and clip flares
#'
#' @param df Dataframe from read_kepler_table
#' @param max_gap_days Maximum time gap before splitting into a new continuous chunk (default: 0.5 days)
#' @param flare_sigma Sigma threshold for positive flare clipping (default: 3.0)
#' @param detrend_window Window size in cadences for running median detrending (default: 49)
#' @return data.frame with cleaned, baseline-flattened flux and chunk IDs
clean_light_curve <- function(df, max_gap_days = 0.5, flare_sigma = 3.0, detrend_window = 49) {
  if(nrow(df) == 0) return(df)
  
  # 1. Sort by time
  df <- df[order(df$time), ]
  
  # 2. Identify gaps and assign chunk IDs
  dt <- c(0, diff(df$time))
  dcad <- c(1, diff(df$cadence))
  
  # Split if time gap > max_gap_days or cadence jump > 24
  is_gap <- (dt > max_gap_days) | (dcad > 24)
  df$chunk_id <- cumsum(is_gap)
  
  # 3. Process each continuous chunk independently
  cleaned_list <- lapply(split(df, df$chunk_id), function(chunk) {
    if(nrow(chunk) < 10) return(NULL)
    
    flux <- chunk$flux
    
    # Detrend low-frequency stellar baseline using running median
    k <- min(detrend_window, nrow(chunk))
    if(k %% 2 == 0) k <- k - 1
    if(k >= 5) {
      baseline <- stats::runmed(flux, k = k, endrule = "median")
    } else {
      baseline <- rep(median(flux, na.rm = TRUE), nrow(chunk))
    }
    
    detrended_flux <- flux - baseline
    
    # Asymmetric positive flare clipping:
    # Exoplanet transits are negative dips; positive spikes are stellar flares / cosmic rays.
    # Clip positive spikes > +flare_sigma * MAD above baseline.
    robust_sd <- stats::mad(detrended_flux, na.rm = TRUE)
    if(is.finite(robust_sd) && robust_sd > 0) {
      upper_limit <- flare_sigma * robust_sd
      detrended_flux[detrended_flux > upper_limit] <- upper_limit
    }
    
    chunk$cleaned_flux <- detrended_flux
    chunk$baseline <- baseline
    return(chunk)
  })
  
  cleaned_df <- do.call(rbind, cleaned_list)
  rownames(cleaned_df) <- NULL
  return(cleaned_df)
}

#' Extract fixed-length sliding windows strictly within continuous chunks
#'
#' @param cleaned_df Dataframe from clean_light_curve
#' @param seq_len Length of sequence window (default: 128 cadences ~ 2.6 days)
#' @param stride Step size between windows (default: 16)
#' @return list containing tensor X (N, seq_len, 1) and metadata
extract_windows <- function(cleaned_df, seq_len = 128, stride = 16) {
  windows <- list()
  meta_list <- list()
  
  if(is.null(cleaned_df) || nrow(cleaned_df) < seq_len) {
    return(list(X = array(0, dim = c(0, seq_len, 1)), meta = data.frame()))
  }
  
  chunks <- split(cleaned_df, cleaned_df$chunk_id)
  
  for(chk in chunks) {
    n_pts <- nrow(chk)
    if(n_pts < seq_len) next
    
    starts <- seq(1, n_pts - seq_len + 1, by = stride)
    for(st in starts) {
      en <- st + seq_len - 1
      sub_flux <- chk$cleaned_flux[st:en]
      
      # Standardize window locally (zero mean, unit variance)
      w_sd <- stats::sd(sub_flux)
      if(is.na(w_sd) || w_sd == 0) w_sd <- 1.0
      w_norm <- (sub_flux - mean(sub_flux)) / w_sd
      
      windows[[length(windows) + 1]] <- w_norm
      meta_list[[length(meta_list) + 1]] <- data.frame(
        chunk_id = chk$chunk_id[1],
        start_time = chk$time[st],
        end_time = chk$time[en],
        orig_start_idx = chk$orig_idx[st],
        orig_end_idx = chk$orig_idx[en]
      )
    }
  }
  
  if(length(windows) == 0) {
    return(list(X = array(0, dim = c(0, seq_len, 1)), meta = data.frame()))
  }
  
  X_mat <- do.call(rbind, windows)
  X_arr <- array(X_mat, dim = c(nrow(X_mat), seq_len, 1))
  meta_df <- do.call(rbind, meta_list)
  
  return(list(X = X_arr, meta = meta_df))
}

#' Legacy compatible get_wave wrapper
#'
#' @param file Path to .tbl file
#' @param impute Ignored, maintained for backward compatibility
#' @return Cleaned flux numeric vector
get_wave <- function(file, impute = TRUE) {
  df <- read_kepler_table(file)
  cleaned <- clean_light_curve(df)
  if(!is.null(cleaned) && nrow(cleaned) > 0) {
    return(cleaned$cleaned_flux)
  }
  return(numeric(0))
}

#' Split continuous light curve chunks into Train and Test sets
#'
#' @param wave Cleaned flux vector or dataframe
#' @param train_ratio Ratio for training set (default: 0.7)
#' @param seq_len Sequence length (default: 128)
#' @return list(x_train, y_train, x_test, y_test)
split_train_test <- function(wave, train_ratio = 0.7, seq_len = 128) {
  if(is.data.frame(wave)) {
    cleaned_df <- wave
  } else {
    cleaned_df <- data.frame(
      time = seq_along(wave) * 0.0204, # ~29.4 min cadences
      cadence = seq_along(wave),
      cleaned_flux = wave,
      chunk_id = 1,
      orig_idx = seq_along(wave)
    )
  }
  
  w_data <- extract_windows(cleaned_df, seq_len = seq_len, stride = max(1, floor(seq_len / 4)))
  X <- w_data$X
  
  if(dim(X)[1] == 0) {
    dummy <- array(0, dim = c(0, seq_len, 1))
    return(list(x_train = dummy, y_train = dummy, x_test = dummy, y_test = dummy))
  }
  
  n_total <- dim(X)[1]
  n_train <- max(1, floor(train_ratio * n_total))
  
  x_train <- X[1:n_train, , , drop = FALSE]
  y_train <- x_train # Autoencoder targets same reconstruction
  
  if(n_train < n_total) {
    x_test <- X[(n_train + 1):n_total, , , drop = FALSE]
    y_test <- x_test
  } else {
    x_test <- x_train
    y_test <- y_train
  }
  
  return(list(x_train = x_train, y_train = y_train, x_test = x_test, y_test = y_test))
}

#' Detect exoplanet transit candidates using one-sided negative flux reconstruction errors
#'
#' @param y_pred Reconstructed/predicted flux array
#' @param y Actual flux array
#' @param sigma_thresh Standard deviation multiplier for anomaly detection (default: 2.5)
#' @param min_duration Minimum transit duration in cadences (default: 2 ~ 1 hour)
#' @param max_duration Maximum transit duration in cadences (default: 24 ~ 12 hours)
#' @return list with anomaly mask and candidate data frame (start, end)
detect_transit_candidates <- function(y_pred, y, sigma_thresh = 2.5, min_duration = 2, max_duration = 24) {
  # Asymmetric error: Exoplanet transits are negative dips (actual < predicted baseline)
  # error = max(0, predicted - actual)
  res_error <- pmax(0, as.vector(y_pred) - as.vector(y))
  
  err_mean <- mean(res_error, na.rm = TRUE)
  err_sd <- stats::sd(res_error, na.rm = TRUE)
  
  if(is.na(err_sd) || err_sd == 0) {
    thr <- Inf
  } else {
    thr <- err_mean + sigma_thresh * err_sd
  }
  
  is_anom <- (res_error > thr) & is.finite(res_error)
  
  # Group contiguous anomalies into candidate transit intervals
  starts <- integer(0)
  ends <- integer(0)
  
  if(any(is_anom)) {
    rle_res <- rle(is_anom)
    end_indices <- cumsum(rle_res$lengths)
    start_indices <- c(1, end_indices[-length(end_indices)] + 1)
    
    true_runs <- which(rle_res$values == TRUE)
    for(r in true_runs) {
      dur <- rle_res$lengths[r]
      if(dur >= min_duration && dur <= max_duration) {
        starts <- c(starts, start_indices[r])
        ends <- c(ends, end_indices[r])
      }
    }
  }
  
  cand_df <- data.frame(start = starts, end = ends)
  return(list(is_anomaly = is_anom, candidates = cand_df, threshold = thr))
}

#' Save comparison plot with highlighted transit candidates
#'
#' @param y_pred Predicted/reconstructed array or vector
#' @param y Actual flux vector
#' @param out_file Output PNG file path
#' @param thr Detection threshold
#' @param lwr Optional lower period bound (maintained for compatibility)
#' @param upr Optional upper period bound (maintained for compatibility)
#' @return data.frame with start and end indices
save_plot <- function(y_pred, y, out_file, thr = NULL, lwr = NULL, upr = NULL) {
  det_res <- detect_transit_candidates(y_pred = y_pred, y = y)
  idx <- det_res$is_anomaly
  df <- det_res$candidates
  
  dir.create(dirname(out_file), showWarnings = FALSE, recursive = TRUE)
  
  png(out_file, width = 1366, height = 768)
  col_vec <- ifelse(idx, "red", "black")
  plot(as.vector(y), col = col_vec, pch = 20, cex = 0.5,
       ylab = "Normalized Flux", xlab = "Cadence Index",
       main = paste("Transit Anomaly Detection -", basename(out_file)))
  lines(as.vector(y_pred), col = "dodgerblue", lwd = 1.5)
  legend("bottomleft", legend = c("Light Curve", "Detected Transit Dip", "Autoencoder Baseline"),
         col = c("black", "red", "dodgerblue"), pch = c(20, 20, NA), lty = c(NA, NA, 1), lwd = c(NA, NA, 2))
  dev.off()
  
  return(df)
}

#' Safe, batch SQLite insertion
#'
#' @param db_file SQLite database path
#' @param table_name Target table name
#' @param idx_df Dataframe to insert
insert_into_db <- function(db_file, table_name, idx_df) {
  if(is.null(idx_df) || nrow(idx_df) == 0) return(invisible(NULL))
  
  mydb <- dbConnect(RSQLite::SQLite(), db_file)
  on.exit(dbDisconnect(mydb), add = TRUE)
  
  dbWriteTable(mydb, table_name, idx_df, append = TRUE)
}
