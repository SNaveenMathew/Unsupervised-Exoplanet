get_period_range <- function(wave) {
  lower <- 2
  upper <- floor(length(wave)/2)
  iter <- 1
  max_iter <- 30
  while(iter <= max_iter) {
    half_width <- (upper-lower)/2
    mid <- floor(lower + half_width)
    lower_ratio <- get_ratio(wave, lower)
    mid_ratio <- get_ratio(wave, mid)
    upper_ratio <- get_ratio(wave, upper)
    if(iter > 1) {
      all_vec <- c(lower_ratio, mid_ratio, upper_ratio)
      if(length(unique(all_vec)) != 1) {
        lowers <- c(floor(lower - half_width), floor(lower + half_width/2), mid)
        uppers <- c(mid, floor(upper - half_width/2), floor(upper + half_width))
        min_idx <- which.min(all_vec)
        lower <- lowers[min_idx]
        upper <- uppers[min_idx]
      } else {
        iter <- max_iter + 1
      }
    } else {
      all_vec <- c(lower_ratio, mid_ratio)
      if(length(unique(all_vec)) != 1) {
        if(lower == min(all_vec)) {
          lower <- floor(lower - half_width)
          upper <- mid
        } else {
          lower <- floor(lower + half_width/2)
          upper <- floor(upper - half_width/2)
        }
      } else {
        iter <- max_iter + 1
      }
    }
    iter <- iter + 1
  }
  if(upper > length(wave)/2) {
    upper <- lower <- floor(length(wave)/2)
  }
  return(c(lower, upper))
}

get_wave <- function(file) {
  temp <- data.frame(read_delim(file, delim = " ", skip = 35, col_names = F),
                     stringsAsFactors = F)
  temp$X8 <- NULL
  temp <- data.frame(sapply(temp, as.numeric))
  cols <- read_lines(file, skip = 31, n_max = 1)
  cols <- strsplit(cols, "\\ {0,}\\|")[[1]][-c(1)]
  colnames(temp) <- cols
  wave <- temp$RESIDUAL_FLUX
  wave <- na.interpolation(wave, option = "stine")
  perc_97.5 <- quantile(wave, 0.975)
  wave[wave > perc_97.5] <- perc_97.5
  return(wave)
}

split_train_test <- function(wave, train_ratio, seq_len) {
  train_len <- floor(train_ratio * length(wave))
  train_dat <- wave[1:train_len]
  test_dat <- wave[(train_len + 1):length(wave)]
  train_arr <- t(sapply(1:(length(train_dat) - seq_len + 1),
                        function(i) return(train_dat[i:(i + seq_len - 1)])))
  test_arr <- t(sapply(1:(length(test_dat) - seq_len + 1),
                       function(i) return(test_dat[i:(i + seq_len - 1)])))
  x_train <- train_arr[, -ncol(train_arr)]
  x_train <- array_reshape(x_train, dim = c(dim(x_train), 1))
  y_train <- train_arr[, ncol(train_arr)]
  x_test <- test_arr[, -ncol(test_arr)]
  x_test <- array_reshape(x_test, dim = c(dim(x_test), 1))
  y_test <- test_arr[, ncol(test_arr)]
  return(list(x_train = x_train, y_train = y_train, x_test = x_test, y_test = y_test))
}

get_ratio <- function(wave, period) {
  brks <- round(length(wave)/period)
  cuts <- cut(1:length(wave), breaks = brks)
  splits <- split(wave, cuts)
  sds <- sapply(splits, sd)
  sds <- sds[sds != 0]
  ratio_max <- max(sds)/min(sds)
  return(ratio_max)
}

save_plot <- function(y_pred, y, out_file, thr = NULL, lwr = NULL, upr = NULL) {
  sqr_error <- (y_pred - y)^2
  if(is.null(thr)) {
    avg_error <- mean(sqr_error)
    std_error <- sd(sqr_error)
    thr <- (avg_error + 2 * std_error)
  }
  
  idx <- sqr_error > thr
  
  if(!is.infinite(thr)) {
    indices <- which(idx)
    starts <- c()
    ends <- c()
    i <- 1
    while(i < (length(indices)-1)) {
      starts <- c(starts, indices[i])
      while(idx[indices[i] + 1, 1] == idx[indices[i], 1] & i < (length(indices)-1)) {
        i <- i + 1
      }
      if(i == (length(indices)-1)) {
        ends <- c(ends, indices[i + 1])
      } else {
        ends <- c(ends, indices[i])
      }
      i <- i + 1
    }
    if(length(starts) > 1) {
      all_diffs <- sapply(ends, function(end) starts-end)
      lst <- lapply(1:length(starts), function(i) starts[i]:ends[i])
      keeps <- apply(all_diffs, 1, function(row) {
        return(any(between(x = abs(row), left = lwr, right = upr)))
      })
      indices <- unlist(lst[keeps])
      idx <- rep(FALSE, length(y))
      idx[indices] <- TRUE
    } else {
      idx <- rep(FALSE, length(y))
      if(length(starts) == 1) {
        idx[starts:ends] <- TRUE
      }
    }
  }
  png(out_file, width = 1366, height = 768)
  plot(y, col = idx + 1)
  dev.off()
}