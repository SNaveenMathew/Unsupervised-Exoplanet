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
#' Normalization statistics (median, MAD) are computed ONCE per chunk, from
#' the whole continuous segment, and applied identically to every window
#' drawn from it - NOT per-window. Per-window mean/sd normalization (the
#' original approach here) breaks the "transits are rare events" assumption
#' anomaly detection depends on, two ways at once: (1) it forces every
#' window to the same mean=0/std=1 scale regardless of content, so a
#' perfectly quiet window's own tiny noise floor gets stretched until some
#' of its points cross the same threshold as a window with a real dip -
#' guaranteeing a roughly constant false-positive rate per window
#' regardless of whether real signal is present; (2) a genuine transit deep
#' or wide enough to influence its own window's mean/std dilutes its own
#' apparent significance (the classic "masking effect" in robust
#' statistics: outliers judged against statistics that include themselves
#' get hidden). Chunk-level median/MAD fixes both: "how anomalous" is now
#' measured against the star's actual noise floor, computed once, robust to
#' the very dips being searched for.
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
    
    # Robust normalization statistics for the WHOLE chunk, computed once -
    # see the function docstring for why this replaces per-window mean/sd.
    chunk_center <- stats::median(chk$cleaned_flux, na.rm = TRUE)
    chunk_scale <- stats::mad(chk$cleaned_flux, na.rm = TRUE)
    if(!is.finite(chunk_scale) || chunk_scale == 0) chunk_scale <- 1.0
    
    starts <- seq(1, n_pts - seq_len + 1, by = stride)
    for(st in starts) {
      en <- st + seq_len - 1
      sub_flux <- chk$cleaned_flux[st:en]
      
      # Same chunk-wide center/scale applied to every window - a window's
      # own content never affects its own normalization.
      w_norm <- (sub_flux - chunk_center) / chunk_scale
      
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

#' Flattens a (n_windows, seq_len, 1) tensor into a 1D vector in
#' chronological order (all of window 1, then all of window 2, ...).
#'
#' R's default as.vector() on an array flattens column-major, meaning the
#' FIRST dimension (window index) varies fastest - so a single window's own
#' seq_len positions end up scattered n_windows apart in the flattened
#' vector, never contiguous. That silently broke every downstream
#' "contiguous run" computation built on top of it: detect_transit_
#' candidates() below, triage_scan(), BLS phase-folding, plotting, and
#' every start/end index ever stored in test_idx/user_star. A transit
#' fully contained within a single window could never register as a
#' multi-point contiguous run, because its own points were never adjacent
#' in the flattened vector to begin with - confirmed directly: the exact
#' same 8-cadence injected dip is found correctly when it's the only
#' window, and missed entirely as soon as it's embedded among others.
#'
#' Falls back to plain as.vector() for anything that isn't a 3D array (e.g.
#' an already-flattened vector), so existing callers passing flat data are
#' unaffected.
#'
#' @param arr A (n_windows, seq_len, 1) array, or any other object
#' @return A flattened numeric vector in chronological order
flatten_chronological <- function(arr) {
  d <- dim(arr)
  if (is.null(d) || length(d) != 3) return(as.vector(arr))
  as.vector(aperm(arr, c(2, 1, 3)))
}

#' Detect exoplanet transit candidates using one-sided negative flux reconstruction errors
#'
#' Thresholding uses robust (median/MAD) statistics on the residual, not
#' ordinary mean/SD. Transits are assumed rare - that's the entire premise
#' of framing this as anomaly detection - but ordinary mean/SD are NOT
#' robust to the very outliers being searched for: a handful of genuine
#' deep transits inflate the SD and raise the threshold, masking real
#' signal (the same "masking effect" fixed in extract_windows()'s
#' normalization). MAD tolerates up to ~50% contamination before breaking
#' down, far more than any real transit duty cycle, so the threshold stays
#' anchored to the star's actual noise floor rather than being dragged
#' around by the anomalies themselves. R's mad() applies the standard
#' 1.4826 consistency constant, so it's on the same numeric scale as SD for
#' near-Gaussian noise - sigma_thresh keeps its usual meaning.
#'
#' @param y_pred Reconstructed/predicted flux array
#' @param y Actual flux array
#' @param sigma_thresh Standard deviation multiplier for anomaly detection (default: 2.5)
#' @param min_duration Minimum transit duration in cadences (default: 2 ~ 1 hour)
#' @param max_duration Maximum transit duration in cadences (default: 24 ~ 12 hours)
#' @param merge_gap Collapses candidates whose starts are within this many
#'   cadences of each other into a single representative candidate (the
#'   first of the group). Overlapping windows (stride < seq_len) mean a
#'   single physical dip near a window boundary gets independently
#'   re-flagged in every subsequent overlapping window that still contains
#'   it, each appearing exactly (seq_len - stride) cadences apart in the
#'   chronologically-flattened vector (see flatten_chronological()) -
#'   verified directly: a real dip on a real star showed up as 4 near-
#'   identical candidates exactly 96 (= 128 - 32) cadences apart, three
#'   separate times. Pass merge_gap = seq_len - stride (score_star() and
#'   pipeline.R both do) to collapse these back into one candidate per
#'   real event; leave at the default 0 for already-deduplicated or
#'   non-windowed input.
#' @return list with anomaly mask and candidate data frame (start, end)
detect_transit_candidates <- function(y_pred, y, sigma_thresh = 2.5, min_duration = 2, max_duration = 24, merge_gap = 0) {
  # Signed, symmetric residual: positive where actual is below predicted
  # (a transit candidate), negative where actual is above it. Flattened
  # chronologically (see flatten_chronological()) so a run of contiguous
  # anomalous cadences within one window actually registers as contiguous,
  # rather than being scattered n_windows apart by R's default as.vector().
  diff_vec <- flatten_chronological(y_pred) - flatten_chronological(y)
  
  # Center and noise scale MUST come from the full symmetric residual
  # above, not from the one-sided clipped res_error below. res_error is
  # exactly zero for roughly half of all points by construction (wherever
  # actual >= predicted), so computing median/MAD directly on it badly
  # underestimates the true noise scale - the same zero-inflation failure
  # mode already fixed in triage_scan() for exactly this reason.
  err_center <- stats::median(diff_vec, na.rm = TRUE)
  err_scale <- stats::mad(diff_vec, na.rm = TRUE)
  
  # Asymmetric error: Exoplanet transits are negative dips (actual < predicted baseline)
  # error = max(0, predicted - actual), centered on the residual's own median
  res_error <- pmax(0, diff_vec - err_center)
  
  if(is.na(err_scale) || err_scale == 0) {
    thr <- Inf
  } else {
    thr <- sigma_thresh * err_scale
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
  
  # Collapse redundant re-detections of the same physical event across
  # overlapping windows - see the merge_gap parameter doc above.
  if (merge_gap > 0 && nrow(cand_df) > 1) {
    cand_df <- cand_df[order(cand_df$start), ]
    gaps <- c(Inf, diff(cand_df$start))  # Inf forces the first row to start a new group
    group_id <- cumsum(gaps > merge_gap)
    cand_df <- cand_df[!duplicated(group_id), ]
    rownames(cand_df) <- NULL
  }
  
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
  
  # Must use the SAME chronological flattening detect_transit_candidates()
  # used internally to build idx/df above - plotting y/y_pred with plain
  # as.vector() here would put the light curve back in scrambled order
  # while idx stays chronological, silently misaligning every highlighted
  # "anomaly" dot with the wrong point.
  y_flat <- flatten_chronological(y)
  y_pred_flat <- flatten_chronological(y_pred)
  
  dir.create(dirname(out_file), showWarnings = FALSE, recursive = TRUE)
  
  png(out_file, width = 1366, height = 768)
  col_vec <- ifelse(idx, "red", "black")
  plot(y_flat, col = col_vec, pch = 20, cex = 0.5,
       ylab = "Normalized Flux", xlab = "Cadence Index",
       main = paste("Transit Anomaly Detection -", basename(out_file)))
  lines(y_pred_flat, col = "dodgerblue", lwd = 1.5)
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
#' @param trained_model_paths Character vector of candidate model file
#'   paths to try in order (e.g. star-specific first, then a global
#'   fallback model). The first one that exists on disk is used. Build
#'   this with dl_model_candidate_paths() so the extension matches the
#'   active backend (.hdf5 for Keras on Linux, .pt for PyTorch on Windows).
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
#' ---------------------------------------------------------------------
#' Cross-platform deep-learning backend (Keras/TensorFlow on Linux,
#' PyTorch on Windows)
#' ---------------------------------------------------------------------
#' TensorFlow dropped native Windows GPU support after 2.10 (Windows users
#' are stuck on an old, unmaintained TF build unless they go through WSL2),
#' while PyTorch maintains full current-CUDA support natively on Windows.
#' Every function below dispatches on dl_backend() so the rest of this
#' file, pipeline.R, and app.Rmd never need to know which one is active -
#' they just call dl_fit()/dl_predict()/dl_save()/dl_load(), and get back
#' the same shapes either way. Both backends are expected to live in the
#' SAME-named Python virtual/conda environment (whatever main.R's
#' reticulate::use_condaenv() call points at) - only which package is
#' installed inside it differs by OS.

#' Which backend this OS should use. Override with the EXOPLANET_DL_BACKEND
#' environment variable (e.g. Sys.setenv(EXOPLANET_DL_BACKEND = "torch") to
#' exercise the PyTorch path from Linux, mainly useful for testing).
#'
#' @return "torch" or "keras"
dl_backend <- function() {
  override <- Sys.getenv("EXOPLANET_DL_BACKEND", "")
  if (nzchar(override)) return(tolower(override))
  if (identical(.Platform$OS.type, "windows")) "torch" else "keras"
}

#' File extension for a saved model under the given (default: current)
#' backend. A model trained with one backend can never be loaded by the
#' other - they're fundamentally different serialization formats - so a
#' star trained on Linux (.hdf5) and then worked on again on Windows
#' (.pt) will simply retrain fresh under the new backend rather than
#' erroring; see dl_model_candidate_paths().
dl_model_ext <- function(backend = dl_backend()) {
  if (identical(backend, "torch")) ".pt" else ".hdf5"
}

#' Candidate model file paths to check for a star, in priority order
#' (star-specific first, then the shared global fallback), using whichever
#' extension matches the active backend. Callers typically call this once
#' per models_dir candidate (e.g. "trained_models" and "../trained_models")
#' and concatenate, mirroring how the two locations are already tried
#' elsewhere in this codebase.
dl_model_candidate_paths <- function(models_dir, out_base, backend = dl_backend()) {
  ext <- dl_model_ext(backend)
  c(
    file.path(models_dir, paste0(out_base, ext)),
    file.path(models_dir, paste0("global_conv1d_autoencoder", ext))
  )
}

#' Whether the active (or given) backend can actually be used right now -
#' the package (keras) or reticulate+the Python module (torch) is
#' installed and importable. Wrapped in tryCatch so a missing/broken
#' Python environment degrades to FALSE rather than erroring.
dl_backend_available <- function(backend = dl_backend()) {
  if (identical(backend, "torch")) {
    isTRUE(tryCatch({
      requireNamespace("reticulate", quietly = TRUE) &&
        reticulate::py_module_available("torch")
    }, error = function(e) FALSE))
  } else {
    requireNamespace("keras", quietly = TRUE)
  }
}

#' Reports (and where possible enables) mixed-precision training for the
#' active backend if a GPU is present. On a GPU with tensor cores (compute
#' capability 7.0+ - any RTX/V100/A-series/H-series card), this roughly
#' doubles the achievable batch size or model capacity for a fixed memory
#' budget, at essentially no accuracy cost for a model this size - directly
#' relevant when training on a single 16GB GPU. Skipped automatically on
#' CPU-only setups, where fp16 ops aren't hardware-accelerated and mixed
#' precision provides no benefit (and can even be slightly slower).
#'
#' The two backends apply this completely differently: Keras/TF uses a
#' single global policy set once, up front (below). PyTorch has no
#' equivalent global switch - autocast is applied per training step inside
#' dl_fit_torch() itself - so for the torch backend this function only
#' detects and reports GPU presence; the returned value is what callers
#' (run_pipeline()) should pass through to dl_fit() as use_amp.
#'
#' @return TRUE if a GPU was detected (and, for Keras, mixed precision was
#'   successfully enabled), FALSE otherwise (invisible)
dl_enable_mixed_precision <- function(backend = dl_backend()) {
  if (identical(backend, "torch")) {
    has_gpu <- tryCatch({
      requireNamespace("reticulate", quietly = TRUE) &&
        reticulate::py_module_available("torch") &&
        isTRUE(reticulate::import("torch")$cuda$is_available())
    }, error = function(e) FALSE)
    if (has_gpu) {
      message("GPU detected - PyTorch will use mixed-precision autocast during training.")
    } else {
      message("No GPU detected - training in default (fp32) precision.")
    }
    return(invisible(has_gpu))
  }

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
#' (Keras/TensorFlow implementation - see build_conv1d_autoencoder_torch()
#' for the PyTorch equivalent; dl_fit() dispatches to whichever is active).
#'
#' @param seq_len Sequence window length (default: 128)
#' @param lr Learning rate for Adam optimizer (default: 0.001)
#' @return Compiled Keras model
build_conv1d_autoencoder_keras <- function(seq_len = 128, lr = 0.001) {
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
    # Forced to float32 even under a mixed_float16 global policy - standard
    # mixed-precision practice: keeping the last layer's activation/loss
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

#' Trains a fresh Keras autoencoder, matching build_conv1d_autoencoder_
#' keras()'s architecture. Returns a history object exposing
#' $metrics$loss/$mae/$val_loss/$val_mae, the same shape dl_fit_torch()
#' below produces, so pipeline.R never needs to know which backend ran.
dl_fit_keras <- function(x_train, y_train, seq_len, batch_size, epochs, lr = 0.001,
                          validation_split = 0.2) {
  model <- build_conv1d_autoencoder_keras(seq_len = seq_len, lr = lr)

  callbacks_list <- list(
    callback_early_stopping(monitor = "val_loss", patience = 5, restore_best_weights = TRUE),
    callback_reduce_lr_on_plateau(monitor = "val_loss", factor = 0.5, patience = 2)
  )

  his <- model %>% fit(
    x = x_train, y = y_train,
    batch_size = min(batch_size, dim(x_train)[1]),
    epochs = epochs,
    validation_split = validation_split,
    callbacks = callbacks_list,
    verbose = 0
  )

  list(model = model, history = his$metrics)
}

#' ---------------------------------------------------------------------
#' PyTorch backend (Windows)
#' ---------------------------------------------------------------------
#' PyTorch has no Keras-style declarative build-then-fit() API, so the
#' model definition and training loop are written as an actual Python
#' script (below) and executed once per session via reticulate::
#' py_run_string(), rather than assembled through many chained reticulate
#' calls into torch$nn$* - far more readable and maintainable than the
#' equivalent would be built up piece by piece from R, and it's the kind
#' of code that's naturally idiomatic in Python anyway (batching loop,
#' autocast context manager, early stopping bookkeeping). R just prepares
#' the data and hyperparameters, calls in, and reads the results back -
#' reticulate converts the R arrays to numpy automatically at the boundary.
#'
#' Mirrors the Keras path's behavior specifically where it matters for
#' parity between backends: validation_split takes the LAST fraction of
#' the (unshuffled) data, matching Keras's own documented behavior, not a
#' random split; early stopping restores the best-validation-loss weights
#' (restore_best_weights=TRUE in the Keras callback); LR reduces on a
#' validation-loss plateau; mixed precision uses torch.autocast + a
#' GradScaler when use_amp is TRUE (passed in by dl_enable_mixed_
#' precision()'s return value), analogous to Keras's global mixed_float16
#' policy. The channels-first/channels-last transpose PyTorch's Conv1d
#' requires versus this codebase's (n_windows, seq_len, 1) convention
#' everywhere else is handled entirely inside this script - nothing
#' outside dl_fit_torch()/dl_predict_torch() ever sees a channels-first
#' array.
.torch_backend_ready <- new.env()
.torch_backend_ready$done <- FALSE

.torch_training_script <- "
import torch
import torch.nn as nn
import numpy as np

def build_autoencoder(seq_len):
    return nn.Sequential(
        nn.Conv1d(1, 32, kernel_size=5, padding='same'), nn.ReLU(),
        nn.MaxPool1d(2),
        nn.Conv1d(32, 64, kernel_size=5, padding='same'), nn.ReLU(),
        nn.MaxPool1d(2),
        nn.Conv1d(64, 128, kernel_size=3, padding='same'), nn.ReLU(),
        nn.MaxPool1d(2),
        nn.Conv1d(128, 128, kernel_size=3, padding='same'), nn.ReLU(),
        nn.Upsample(scale_factor=2, mode='nearest'),
        nn.Conv1d(128, 64, kernel_size=5, padding='same'), nn.ReLU(),
        nn.Upsample(scale_factor=2, mode='nearest'),
        nn.Conv1d(64, 32, kernel_size=5, padding='same'), nn.ReLU(),
        nn.Upsample(scale_factor=2, mode='nearest'),
        nn.Conv1d(32, 1, kernel_size=5, padding='same'),
    )

def _to_channels_first(x):
    t = torch.from_numpy(np.asarray(x, dtype=np.float32))
    return t.permute(0, 2, 1).contiguous()

def _to_channels_last_numpy(t):
    return t.permute(0, 2, 1).contiguous().cpu().numpy()

def train_autoencoder(x_train, y_train, seq_len, epochs=20, batch_size=256, lr=0.001,
                       patience=5, lr_patience=2, lr_factor=0.5, val_split=0.2,
                       use_amp=False, seed=0):
    torch.manual_seed(seed)
    device = torch.device('cuda' if torch.cuda.is_available() else 'cpu')

    x = _to_channels_first(x_train).to(device)
    y = _to_channels_first(y_train).to(device)

    n = x.shape[0]
    n_val = max(1, int(n * val_split)) if n > 1 else 0
    n_tr = max(1, n - n_val)
    # Keras's validation_split takes the LAST fraction of the (unshuffled)
    # data, not a random split - matched here for behavioral parity.
    x_tr, y_tr = x[:n_tr], y[:n_tr]
    x_val, y_val = (x[n_tr:], y[n_tr:]) if n_val > 0 and n_tr < n else (None, None)

    model = build_autoencoder(seq_len).to(device)
    optimizer = torch.optim.Adam(model.parameters(), lr=lr)
    loss_fn = nn.MSELoss()
    mae_fn = nn.L1Loss()
    scheduler = torch.optim.lr_scheduler.ReduceLROnPlateau(
        optimizer, mode='min', factor=lr_factor, patience=lr_patience)
    amp_enabled = bool(use_amp) and device.type == 'cuda'
    scaler = torch.cuda.amp.GradScaler(enabled=amp_enabled)

    history = {'loss': [], 'mae': [], 'val_loss': [], 'val_mae': []}
    best_val = float('inf')
    best_state = None
    epochs_no_improve = 0
    n_batches = max(1, (n_tr + batch_size - 1) // batch_size)

    for epoch in range(int(epochs)):
        model.train()
        perm = torch.randperm(n_tr, device=device)
        running_loss = 0.0
        running_mae = 0.0
        for b in range(n_batches):
            idx = perm[b*batch_size:(b+1)*batch_size]
            if idx.numel() == 0:
                continue
            xb, yb = x_tr[idx], y_tr[idx]
            optimizer.zero_grad()
            with torch.autocast(device_type=device.type, enabled=amp_enabled):
                pred = model(xb)
                loss = loss_fn(pred, yb)
            scaler.scale(loss).backward()
            scaler.step(optimizer)
            scaler.update()
            running_loss += loss.item() * xb.shape[0]
            running_mae += mae_fn(pred, yb).item() * xb.shape[0]

        train_loss = running_loss / n_tr
        train_mae = running_mae / n_tr

        if x_val is not None:
            model.eval()
            with torch.no_grad():
                val_pred = model(x_val)
                val_loss = loss_fn(val_pred, y_val).item()
                val_mae = mae_fn(val_pred, y_val).item()
        else:
            val_loss, val_mae = train_loss, train_mae

        history['loss'].append(train_loss)
        history['mae'].append(train_mae)
        history['val_loss'].append(val_loss)
        history['val_mae'].append(val_mae)

        scheduler.step(val_loss)

        if val_loss < best_val - 1e-7:
            best_val = val_loss
            best_state = {k: v.detach().clone() for k, v in model.state_dict().items()}
            epochs_no_improve = 0
        else:
            epochs_no_improve += 1
            if epochs_no_improve >= patience:
                break

    if best_state is not None:
        model.load_state_dict(best_state)

    return model, history

def predict_autoencoder(model, x):
    device = next(model.parameters()).device
    model.eval()
    with torch.no_grad():
        xt = _to_channels_first(x).to(device)
        pred = model(xt)
    return _to_channels_last_numpy(pred)

def save_autoencoder(model, path):
    torch.save(model, path)

def load_autoencoder(path):
    device = torch.device('cuda' if torch.cuda.is_available() else 'cpu')
    m = torch.load(path, map_location=device, weights_only=False)
    m.eval()
    return m
"

#' Defines the Python-side functions in .torch_training_script (once per R
#' session) via reticulate. Every dl_*_torch() function calls this first.
ensure_torch_backend <- function() {
  if (isTRUE(.torch_backend_ready$done)) return(invisible(TRUE))
  reticulate::py_run_string(.torch_training_script)
  .torch_backend_ready$done <- TRUE
  invisible(TRUE)
}

#' Untrained PyTorch autoencoder matching build_conv1d_autoencoder_keras()'s
#' architecture. Exists for symmetry/standalone use; dl_fit_torch() builds
#' and trains its own model internally (PyTorch has no separate compile()
#' step), so ordinary training never needs to call this directly.
build_conv1d_autoencoder_torch <- function(seq_len = 128) {
  ensure_torch_backend()
  reticulate::py$build_autoencoder(as.integer(seq_len))
}

#' Trains a fresh PyTorch autoencoder. Returns list(model, history) with
#' the same $loss/$mae/$val_loss/$val_mae shape dl_fit_keras() produces.
#'
#' @param use_amp Whether to use mixed-precision autocast (only takes
#'   effect on a CUDA device; silently ignored on CPU). Pass the return
#'   value of dl_enable_mixed_precision() here.
dl_fit_torch <- function(x_train, y_train, seq_len, batch_size, epochs, lr = 0.001,
                          validation_split = 0.2, use_amp = FALSE) {
  ensure_torch_backend()
  result <- reticulate::py$train_autoencoder(
    x_train = x_train, y_train = y_train, seq_len = as.integer(seq_len),
    epochs = as.integer(epochs), batch_size = as.integer(batch_size), lr = lr,
    val_split = validation_split, use_amp = isTRUE(use_amp)
  )
  history <- result[[2]]
  list(model = result[[1]], history = list(
    loss = unlist(history$loss), mae = unlist(history$mae),
    val_loss = unlist(history$val_loss), val_mae = unlist(history$val_mae)
  ))
}

dl_predict_torch <- function(model, x) {
  ensure_torch_backend()
  reticulate::py$predict_autoencoder(model, x)
}

dl_save_torch <- function(model, filepath) {
  ensure_torch_backend()
  reticulate::py$save_autoencoder(model, filepath)
}

dl_load_torch <- function(filepath) {
  ensure_torch_backend()
  reticulate::py$load_autoencoder(filepath)
}

#' ---------------------------------------------------------------------
#' Unified dispatchers - pipeline.R and score_star() call only these
#' ---------------------------------------------------------------------

#' Builds AND trains a fresh autoencoder, dispatching to Keras or PyTorch.
#' Always returns list(model, history) where history has $loss, $mae,
#' $val_loss, $val_mae (one value per epoch actually run), regardless of
#' backend.
dl_fit <- function(x_train, y_train, seq_len = 128, batch_size = 256, epochs = 20,
                    lr = 0.001, validation_split = 0.2, use_amp = FALSE,
                    backend = dl_backend()) {
  if (identical(backend, "torch")) {
    dl_fit_torch(x_train, y_train, seq_len, batch_size, epochs, lr, validation_split, use_amp)
  } else {
    dl_fit_keras(x_train, y_train, seq_len, batch_size, epochs, lr, validation_split)
  }
}

#' Runs inference with an already-loaded model, dispatching to Keras or
#' PyTorch. Always takes/returns the same (n_windows, seq_len, 1) shape
#' used everywhere else in this codebase.
dl_predict <- function(model, x, backend = dl_backend()) {
  if (identical(backend, "torch")) dl_predict_torch(model, x) else predict(model, x, verbose = 0)
}

#' Saves a trained model to filepath, dispatching to Keras (.hdf5) or
#' PyTorch (.pt) based on the extension dl_model_ext() already put there.
dl_save <- function(model, filepath, backend = dl_backend()) {
  if (identical(backend, "torch")) dl_save_torch(model, filepath) else keras::save_model_hdf5(model, filepath)
}

#' Loads a previously-saved model from filepath, dispatching the same way
#' dl_save() does.
dl_load <- function(filepath, backend = dl_backend()) {
  if (identical(backend, "torch")) dl_load_torch(filepath) else keras::load_model_hdf5(filepath)
}

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
  # Chronologically-ordered flattening (see flatten_chronological()) - NOT
  # plain as.vector(), which would scatter each window's own points
  # n_windows apart and silently break every "contiguous run" computation
  # downstream (triage, detection, BLS, plotting) that consumes test_vec.
  test_vec <- flatten_chronological(y_test)
  
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
    } else if (!dl_backend_available()) {
      fallback_reason <- "no_backend"
    } else {
      pred_or_err <- tryCatch({
        model <- dl_load(valid_mdl)
        dl_predict(model, x_test)
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
  # of the codebase computed them. merge_gap = seq_len - stride collapses
  # redundant re-detections of the same physical event across overlapping
  # windows (see detect_transit_candidates()'s merge_gap docs) - stride
  # here matches split_train_test()'s own stride = max(1, floor(seq_len/4)).
  rec <- record_candidates(
    y_pred = y_pred, y = y_test, star_id = star_id,
    db_conn = db_conn, table_name = "test_idx",
    plot_file = plot_file, force = force,
    merge_gap = seq_len - max(1, floor(seq_len / 4))
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
#' Every row written by the model includes the same `label` and
#' `classified_at` columns used by human tags in `user_star`, so the two
#' sources are structurally identical. Model rows are distinguished from
#' human rows purely by *which table* they land in (`test_idx`/`train_idx`
#' vs `user_star`), not by schema differences.
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
#' @param merge_gap Passed straight through to detect_transit_candidates()
#'   - see its docs. Callers using overlapping windows should pass
#'   seq_len - stride so redundant re-detections of the same physical
#'   event collapse into one candidate.
#' @param label Classification label written to every row. Defaults to
#'   "model_detection" for autoencoder-detected windows; callers may pass
#'   a different string (e.g. "model_train_detection") if they need to
#'   distinguish train vs test detections beyond the table_name.
#' @return list(candidates, from_cache)
record_candidates <- function(y_pred, y, star_id, db_conn = NULL, table_name = "test_idx",
                               plot_file = NULL, force = FALSE, merge_gap = 0,
                               label = "model_detection") {
  det_res <- detect_transit_candidates(y_pred = y_pred, y = y, merge_gap = merge_gap)
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
      ts <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S")
      cand_out <- if (nrow(candidates) > 0) {
        out <- candidates
        out$id    <- star_id
        out$label <- label
        out$classified_at <- ts
        out[, c("id", "start", "end", "label", "classified_at")]
      } else {
        # placeholder: "scored, nothing found" — same (0,0) convention as user_star
        data.frame(id = star_id, start = 0L, end = 0L,
                   label = label, classified_at = ts, stringsAsFactors = FALSE)
      }
      # Ensure new columns exist in the target table. ALTER TABLE ADD COLUMN errors
      # if the column already exists; we swallow that so re-runs are idempotent.
      for (.col_def in c("label TEXT", "classified_at TEXT")) {
        tryCatch(
          dbExecute(db_conn, sprintf("ALTER TABLE \"%s\" ADD COLUMN %s;", table_name, .col_def)),
          error = function(e) NULL
        )
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
