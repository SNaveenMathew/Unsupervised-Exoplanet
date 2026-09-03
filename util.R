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

#' Fast, model-free triage scan for whether a light curve segment shows any
#' statistically plausible transit-like dip, so the (comparatively expensive)
#' autoencoder inference can be skipped entirely when there's clearly nothing
#' to find. Deliberately tuned toward HIGH RECALL (few false "nothing here"
#' verdicts) at the cost of some false positives passing through to the full
#' model: the cost of a false pass-through is one extra (cheap-by-comparison)
#' inference call, while the cost of a false skip is a missed transit, which
#' is much worse. This is a threshold rule, not a learned classifier - it
#' will not match the skip rate of a trained triage model (e.g. AstroNet-
#' Triage), but the worth_full_scan interface here is architecture-agnostic,
#' so a learned classifier can be swapped in later without touching callers.
#'
#' @param flux Cleaned, detrended flux vector (e.g. the flattened y_test from
#'   split_train_test())
#' @param sigma_thresh MAD-based sigma threshold for a candidate dip point
#'   (default 3.0 - deliberately looser than detect_transit_candidates()'s
#'   default 2.5-on-reconstruction-error, since raw flux is noisier than a
#'   model's residual and we'd rather over-pass than under-pass)
#' @param min_duration,max_duration Candidate run length bounds, in cadences.
#'   max_duration is intentionally wider than detect_transit_candidates()'s
#'   default, since the true transit width isn't known yet at this stage.
#' @param min_points Minimum number of finite points required to run the
#'   scan at all; below this, default to worth_full_scan = TRUE (too little
#'   data to safely skip on).
#' @return list(worth_full_scan, n_candidate_points, candidate_windows)
triage_scan <- function(flux, sigma_thresh = 3.0, min_duration = 2, max_duration = 48, min_points = 20) {
  fl <- as.vector(flux)
  ok <- is.finite(fl)
  if (sum(ok) < min_points) {
    return(list(worth_full_scan = TRUE, n_candidate_points = NA_integer_, candidate_windows = list()))
  }
  
  baseline <- stats::median(fl[ok])
  # Noise scale must come from the full, symmetric residual - NOT from the
  # one-sided pmax(0, ...) dip values below, which are mostly zero and would
  # badly underestimate the true noise scale (most of a symmetric noise
  # distribution gets clipped away, collapsing its MAD toward 0 and making
  # the resulting threshold far too loose).
  robust_sd <- stats::mad(fl[ok] - baseline, na.rm = TRUE)
  dip <- rep(0, length(fl))
  dip[ok] <- pmax(0, baseline - fl[ok])  # positive during a negative flux dip
  
  if (!is.finite(robust_sd) || robust_sd <= 0) {
    return(list(worth_full_scan = TRUE, n_candidate_points = NA_integer_, candidate_windows = list()))
  }
  
  is_cand <- ok & (dip > sigma_thresh * robust_sd)
  
  starts <- integer(0); ends <- integer(0)
  if (any(is_cand)) {
    rle_res <- rle(is_cand)
    end_idx <- cumsum(rle_res$lengths)
    start_idx <- c(1, end_idx[-length(end_idx)] + 1)
    true_runs <- which(rle_res$values == TRUE)
    for (r in true_runs) {
      dur <- rle_res$lengths[r]
      if (dur >= min_duration && dur <= max_duration) {
        starts <- c(starts, start_idx[r])
        ends <- c(ends, end_idx[r])
      }
    }
  }
  
  windows <- if (length(starts) > 0) lapply(seq_along(starts), function(i) c(starts[i], ends[i])) else list()
  
  list(
    worth_full_scan = length(windows) > 0,
    n_candidate_points = sum(is_cand),
    candidate_windows = windows
  )
}

#' Score a single star end to end: read + clean + window the light curve,
#' run the fast triage scan, then (only if triage says it's worth it) load a
#' trained model and predict, then detect transit candidates and cache the
#' result. This is the single source of truth for "what does this star's
#' result look like" - shared by the Shiny app's on-demand view
#' (output$trainPlot) and the standalone precompute_all_stars.R batch script,
#' so the two can never silently compute a star's result differently.
#'
#' Deliberately has NO dependency on Shiny (no reactives, no reactiveVal
#' writes) so it can run in a plain Rscript context. The caller is
#' responsible for surfacing fallback_reason/triage in whatever UI it has.
#'
#' @param tbl_file Path to the star's .tbl file
#' @param trained_model_paths Character vector of candidate .hdf5 paths to
#'   try in order (e.g. star-specific first, then a global fallback model).
#'   The first one that exists on disk is used.
#' @param db_conn Open DBI connection to the exoplanet_db.sqlite database
#'   (for the test_idx cache table). Pass NULL to skip DB caching entirely
#'   (e.g. a dry run).
#' @param plot_file Optional PNG path; if given and it doesn't already exist,
#'   a comparison plot is written there via save_plot() (matching the Shiny
#'   app's on-disk plot cache). Pass NULL to skip plot generation, which the
#'   batch script does by default so it doesn't litter plots/ with thousands
#'   of PNGs.
#' @param seq_len,train_ratio,cadence_days As used elsewhere in the pipeline
#' @param triage_sigma Sigma threshold passed through to triage_scan()
#' @param force If TRUE, recompute and overwrite any existing test_idx rows
#'   for this star even if it's already cached
#' @return list(kepler_id, out_base, y_test, x_test, test_vec, y_pred,
#'   candidates, fallback_reason, triage, from_cache)
score_star <- function(tbl_file, trained_model_paths, db_conn = NULL, plot_file = NULL,
                        seq_len = 128, train_ratio = 0.7, cadence_days = 29.4 / 1440,
                        triage_sigma = 3.0, force = FALSE) {
  out_base <- tools::file_path_sans_ext(basename(tbl_file))
  star_id <- suppressWarnings(as.integer(gsub("^kplr([0-9]+).*", "\\1", out_base)))
  
  raw_df <- read_kepler_table(tbl_file)
  cleaned_df <- clean_light_curve(raw_df)
  split_res <- split_train_test(cleaned_df, train_ratio = train_ratio, seq_len = seq_len)
  y_test <- split_res$y_test
  x_test <- split_res$x_test
  test_vec <- as.vector(y_test)
  
  # --- Stage 1: fast, model-free triage scan ---
  triage <- triage_scan(test_vec, sigma_thresh = triage_sigma)
  
  y_pred <- array(0, dim = dim(y_test))
  fallback_reason <- NULL
  
  if (!isTRUE(triage$worth_full_scan)) {
    # Triage found nothing worth a closer look - skip model load/inference
    # entirely. This is an intentional, expected zero result (compute
    # saved), not a degraded one, and should be surfaced to the user as such
    # rather than lumped in with the "couldn't get a real prediction"
    # reasons below.
    fallback_reason <- "triage_skip"
  } else {
    valid_mdl <- trained_model_paths[file.exists(trained_model_paths)][1]
    if (is.na(valid_mdl)) {
      fallback_reason <- "no_model"
    } else if (!requireNamespace("keras", quietly = TRUE)) {
      fallback_reason <- "no_keras"
    } else {
      pred_or_err <- tryCatch({
        model <- keras::load_model_hdf5(valid_mdl)
        predict(model, x_test, verbose = 0)
      }, error = function(e) e)
      if (inherits(pred_or_err, "error")) {
        fallback_reason <- "load_error"
      } else {
        y_pred <- pred_or_err
      }
    }
  }
  
  # detect_transit_candidates() + save_plot() + DB caching are all handled
  # by record_candidates() below - the same function pipeline.R's training
  # loop calls for both its train_idx and test_idx writes, so a star's
  # recorded candidates can never silently differ depending on which part
  # of the codebase computed them.
  rec <- record_candidates(
    y_pred = y_pred, y = y_test, star_id = star_id,
    db_conn = db_conn, table_name = "test_idx",
    plot_file = plot_file, force = force
  )
  
  list(
    kepler_id = star_id, out_base = out_base,
    y_test = y_test, x_test = x_test, test_vec = test_vec,
    y_pred = y_pred, candidates = rec$candidates,
    fallback_reason = fallback_reason, triage = triage,
    from_cache = rec$from_cache
  )
}

#' Detects transit candidates for a (prediction, actual) pair and records
#' them into the given SQLite table. This is the single shared
#' implementation of "what do we write to the database for a star's
#' candidates" - used by score_star() above (for the Shiny app and
#' precompute_all_stars.R) AND by pipeline.R's training loop (for both
#' train_idx and test_idx), so the two can never silently disagree.
#'
#' A star with genuinely zero detected candidates still gets ONE row
#' written (start = 0, end = 0 - the same "nothing found" placeholder
#' convention app.Rmd already uses for an explicit "no transit" human tag)
#' rather than no row at all. Otherwise a star that was scored and truly
#' has no candidates would look identical, from a cache-presence check, to
#' a star that was never scored at all - and would get silently recomputed
#' forever by any caller that checks cache presence via row count (as
#' precompute_all_stars.R does).
#'
#' @param y_pred,y Predicted/reconstructed and actual arrays (same shape)
#' @param star_id Integer Kepler ID
#' @param db_conn Open DBI connection. Pass NULL to skip the DB write
#'   entirely (candidates are still computed and returned).
#' @param table_name Target table, e.g. "test_idx" or "train_idx"
#' @param plot_file Optional PNG path; written via save_plot() if given and
#'   not already on disk.
#' @param force If TRUE, delete any existing rows for this star in
#'   table_name first (recompute/overwrite). If FALSE (default) and rows
#'   already exist, nothing is written and from_cache = TRUE.
#' @return list(candidates, from_cache)
record_candidates <- function(y_pred, y, star_id, db_conn = NULL, table_name = "test_idx",
                               plot_file = NULL, force = FALSE) {
  det_res <- detect_transit_candidates(y_pred = y_pred, y = y)
  candidates <- det_res$candidates
  
  if (!is.null(plot_file) && !file.exists(plot_file)) {
    tryCatch(save_plot(y_pred = y_pred, y = y, out_file = plot_file), error = function(e) NULL)
  }
  
  from_cache <- FALSE
  if (!is.null(db_conn) && is.finite(star_id)) {
    existing <- tryCatch({
      dbGetQuery(db_conn, sprintf("SELECT count(*) AS n FROM %s WHERE id = ?;", table_name),
                 params = list(star_id))$n
    }, error = function(e) 0L)
    existing <- if (length(existing) == 0) 0L else existing
    
    if (!force && existing > 0) {
      from_cache <- TRUE
    } else {
      if (force && existing > 0) {
        dbExecute(db_conn, sprintf("DELETE FROM %s WHERE id = ?;", table_name), params = list(star_id))
      }
      cand_out <- if (nrow(candidates) > 0) {
        out <- candidates
        out$id <- star_id
        out[, c("id", "start", "end")]
      } else {
        data.frame(id = star_id, start = 0, end = 0)  # placeholder: "scored, nothing found"
      }
      dbWriteTable(db_conn, table_name, cand_out, append = TRUE)
    }
  }
  
  list(candidates = candidates, from_cache = from_cache)
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
