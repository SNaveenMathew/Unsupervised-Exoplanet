# Note: Keras uses multiple cores. Therefore multiprocessing is not used

library(readr)
library(imputeTS)
library(reticulate)
reticulate::use_condaenv("tf_gpu")
library(keras)
library(kerasR)
library(imputeTS)
library(reshape2)
library(dplyr)

set.seed(1)

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
        if(lower_ratio == min(all_vec)) {
          lower <- floor(lower - half_width)
          upper <- mid
        } else if(mid_ratio == min(all_vec)) {
          lower <- floor(lower + half_width/2)
          upper <- floor(upper - half_width/2)
        } else {
          lower <- mid
          upper <- floor(upper + half_width)
        }
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

split_train_test <- function(wave, train_ratio) {
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
      idx <- rep(FALSE, length(y_test))
      idx[indices] <- TRUE
    } else {
      idx <- rep(FALSE, length(y_test))
      idx[starts:ends] <- TRUE
    }
  }
  png(out_file, width = 1366, height = 768)
  plot(y_test, col = idx + 1)
  dev.off()
}

seq_len <- 10
train_ratio <- 0.7
batch_size <- 50
epochs <- 10
shp <- c(seq_len - 1, 1)

reduceLr <- callback_reduce_lr_on_plateau(
  monitor = "val_loss", factor = sqrt(0.1),
  patience = 2)
# Early stopping is not working for some reason
# earlyStopping <- EarlyStopping(monitor = "val_acc", min_delta = -0.001,
#                                patience = 2)

files <- list.files(path = "data/", pattern = "*.tbl", full.names = T)
system("sh counts.sh")
text <- system("sh counts_df.sh", intern = T)
text <- t(sapply(strsplit(text, "\t"), function(elem) c(elem[1], elem[2])))
text <- data.frame(text)
text$X2 <- as.numeric(text$X2)
out_files <- sapply(strsplit(files, "/"), function(strs) strs[length(strs)])
out_files <- sapply(strsplit(out_files, "\\."), function(strs) strs[1])
dir.create("plots", showWarnings = F)
setwd("plots")
dir.create("learning_curve", showWarnings = F)
dir.create("test_pred_plot", showWarnings = F)
setwd("../")

# Randomizing the input files
rand_idx <- sample(x = 1:length(files), size = length(files), replace = F)
files <- files[rand_idx]
out_files <- out_files[rand_idx]

tm <- Sys.time()
i <- 1
run_hrs <- 3
diff_time <- 0
metrics_df <- data.frame()
while(i <= length(files) & diff_time < 3600 * run_hrs) {
  print(i)
  file <- files[i]
  wave <- get_wave(file)
  train_test_split <- split_train_test(wave, train_ratio)
  x_train <- train_test_split$x_train
  y_train <- train_test_split$y_train
  x_test <- train_test_split$x_test
  y_test <- train_test_split$y_test
  mdl_file <- paste0("trained_models/", out_files[i], ".hdf5")
  if(!file.exists(mdl_file)) {
    model <- keras_model_sequential() %>%
      layer_lstm(input_shape = shp, units = 64, return_sequences = T) %>%
      layer_dropout(0.2) %>%
      layer_lstm(256, return_sequences = T) %>%
      layer_dropout(0.2) %>%
      layer_lstm(100, return_sequences = T) %>%
      layer_dropout(0.2) %>%
      layer_flatten() %>%
      layer_dense(units = 1, activation = "linear")
    model %>% compile(loss = "mse", optimizer = optimizer_adam(lr = 0.1),
                      metrics = c("mape", "mae"))
    his <- model %>% fit(
      x_train, y_train,
      batch_size = batch_size,
      epochs = epochs,
      callbacks = list(reduceLr),
      validation_split = 0.3,
      verbose = 0
    )
    save_model_hdf5(model, paste0("trained_models/", out_files[i], ".hdf5"))
    png(paste0("plots/learning_curve/", out_files[i], "_learning.png"),
        width = 1366, height = 768)
    print(plot(his))
    dev.off()
  } else {
    model <- load_model_hdf5(mdl_file)
  }
  lwr_upr <- get_period_range(wave)
  lwr <- lwr_upr[1]
  upr <- lwr_upr[2]
  if(lwr == upr) {
    thr <- as.numeric("Inf")
  } else {
    y_train_pred <- predict(model, x_train)
    sqr_error <- (y_train_pred - y_train)^2
    avg_error <- mean(sqr_error)
    std_error <- sd(sqr_error)
    thr <- (avg_error + 2 * std_error)
  }
  y_test_pred <- predict(model, x_test)
  save_plot(y_pred = y_test_pred, y = y_test,
            out_file = paste0("plots/test_pred_plot/", out_files[i],
                              "_test_plot.png"),
            thr = thr, lwr = lwr, upr = upr)
  tm1 <- Sys.time()
  diff_time <- as.double.difftime(tm1 - tm, units = "secs")
  metrics_df <- rbind(metrics_df, data.frame(
    train_loss = his$metrics$loss[10],
    train_mape = his$metrics$mean_absolute_percentage_error[10],
    train_mae = his$metrics$mean_absolute_error[10],
    val_loss = his$metrics$val_loss[10],
    val_mape = his$metrics$val_mean_absolute_percentage_error[10],
    val_mae = his$metrics$val_mean_absolute_error[10]))
  i <- i + 1
}

metrics_df$file <- files[1:(i - 1)]

png("Presentation/example.png")
wave <- get_wave(file)
plot(wave)
mn <- mean(wave, na.rm = T)
stdev <- sd(wave, na.rm = T)
lns1 <- mn + c(-1, 1) * stdev
lns2 <- mn + c(-2, 2) * stdev
lns3 <- mn + c(-3, 3) * stdev
abline(h = lns1, col = "red")
abline(h = lns2, col = "green")
abline(h = lns3, col = "blue")
legend("bottomleft", legend = c("1 * std", "2 * std", "3 * std"),
       fill = c("red", "green", "blue"))
dev.off()

met_df <- melt(data = metrics_df, id = "file")
png("Report/loss.png", width = 554, height = 412)
loss_df <- met_df[grep(met_df$variable, pattern =  ".*_loss$"), ]
with(loss_df,
     plot(x = as.integer(factor(file)), y = value, col = variable, pch = 20,
          xlab = "File", xaxt = 'n'))
legend("topright", legend = unique(loss_df$variable), fill = unique(loss_df$variable))
dev.off()
png("Report/mae.png", width = 554, height = 412)
mae_df <- met_df[grep(met_df$variable, pattern =  ".*_mae$"), ]
with(mae_df,
     plot(x = as.integer(factor(file)), y = value, col = variable, pch = 20,
          xlab = "File", xaxt = 'n'))
legend("topright", legend = unique(mae_df$variable), fill = unique(mae_df$variable))
dev.off()
png("Report/mape.png", width = 554, height = 412)
mape_df <- met_df[grep(met_df$variable, pattern =  ".*_mape$"), ]
with(mape_df,
     plot(x = as.integer(factor(file)), y = value, col = variable, pch = 20,
          xlab = "File", xaxt = 'n'))
legend("topright", legend = unique(mape_df$variable), fill = unique(mape_df$variable))
dev.off()

# # Phase folding on chosen files:
# 
# file <- files[1]
# mdl_file <- paste0("trained_models/", out_files[1], ".hdf5")
# 
# 
# file <- "data/kplr010074700_q1_q16_tce_01_dvt_lc.tbl"
# mdl_file <- "trained_models/kplr010074700_q1_q16_tce_01_dvt_lc.hdf5"
