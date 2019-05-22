run_pipeline <- function(data_dir = "data") {
  unlink("exoplanet_db.sqlite")
  mydb <- dbConnect(RSQLite::SQLite(), "exoplanet_db.sqlite")
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
  
  files <- list.files(path = paste0(data_dir, "/"), pattern = "*.tbl",
                      full.names = T)
  system("sh counts.sh")
  text <- system("sh counts_df.sh", intern = T)
  text <- t(sapply(strsplit(text, "\t"), function(elem) c(elem[1], elem[2])))
  text <- data.frame(text, stringsAsFactors = F)
  text$X1 <- as.integer(text$X1)
  text$X2 <- as.numeric(text$X2)
  colnames(text) = c("id", "num_planets")
  dbWriteTable(mydb, "kepler_star", text)
  out_files <- sapply(strsplit(files, "/"), function(strs) strs[length(strs)])
  out_files <- sapply(strsplit(out_files, "\\."), function(strs) strs[1])
  out_files1 <- sapply(strsplit(out_files, "_"), function(strs) strs[1])
  out_files1 <- as.integer(gsub(x = out_files1, pattern = "kplr", replacement = ""))
  dir.create("plots", showWarnings = F)
  setwd("plots")
  dir.create("learning_curve", showWarnings = F)
  dir.create("test_pred_plot", showWarnings = F)
  dir.create("train_pred_plot", showWarnings = F)
  setwd("../")
  dir.create("trained_models", showWarnings = F)
  
  # Randomizing the input files
  rand_idx <- sample(x = 1:length(files), size = length(files), replace = F)
  files <- files[rand_idx]
  out_files <- out_files[rand_idx]
  out_files1 <- out_files1[rand_idx]
  
  tm <- Sys.time()
  i <- 1
  run_hrs <- 8
  diff_time <- 0
  metrics_df <- data.frame()
  train_idx_df <- data.frame()
  test_idx_df <- data.frame()
  while(i <= length(files) & diff_time < 3600 * run_hrs) {
    tryCatch({
      print(i)
      file <- files[i]
      wave <- get_wave(file)
      train_test_split <- split_train_test(wave, train_ratio, seq_len)
      x_train <- train_test_split$x_train
      y_train <- train_test_split$y_train
      x_test <- train_test_split$x_test
      y_test <- train_test_split$y_test
      mdl_file <- paste0("trained_models/", out_files[i], ".hdf5")
      if(!file.exists(mdl_file)) {
        model <- keras_model_sequential() %>%
          # layer_cudnn_lstm(input_shape = shp, units = 64, return_sequences = T) %>%
          layer_lstm(input_shape = shp, units = 64, return_sequences = T) %>%
          layer_dropout(0.2) %>%
          # layer_cudnn_lstm(256, return_sequences = T) %>%
          layer_lstm(256, return_sequences = T) %>%
          layer_dropout(0.2) %>%
          # layer_cudnn_lstm(100, return_sequences = T) %>%
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
        metrics_df <- rbind(metrics_df, data.frame(
          train_loss = his$metrics$loss[epochs],
          train_mape = his$metrics$mean_absolute_percentage_error[epochs],
          train_mae = his$metrics$mean_absolute_error[epochs],
          val_loss = his$metrics$val_loss[epochs],
          val_mape = his$metrics$val_mean_absolute_percentage_error[epochs],
          val_mae = his$metrics$val_mean_absolute_error[epochs]))
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
      y_train_pred <- predict(model, x_train)
      temp_train_idx_df <- save_plot(
        y_pred = y_train_pred, y = y_train,
        out_file = paste0("plots/train_pred_plot/", out_files[i],
                          "_train_plot.png"),
        thr = thr, lwr = lwr, upr = upr)
      if(nrow(temp_train_idx_df) > 0) {
        temp_train_idx_df$id <- out_files1[i]
        temp_train_idx_df <- temp_train_idx_df[, c("id", "start", "end")]
      } else {
        temp_train_idx_df <- data.frame(id = out_files1, start = 0, end = 0)
      }
      train_idx_df <- rbind(train_idx_df, temp_train_idx_df)
      y_test_pred <- predict(model, x_test)
      temp_test_idx_df <- save_plot(
        y_pred = y_test_pred, y = y_test,
        out_file = paste0("plots/test_pred_plot/", out_files[i],
                          "_test_plot.png"),
        thr = thr, lwr = lwr, upr = upr)
      if(nrow(temp_test_idx_df) > 0) {
        temp_test_idx_df$id <- out_files1[i]
        temp_test_idx_df <- temp_test_idx_df[, c("id", "start", "end")]
      } else {
        temp_test_idx_df <- data.frame(id = out_files1, start = 0, end = 0)
      }
      test_idx_df <- rbind(test_idx_df, temp_test_idx_df)
      tm1 <- Sys.time()
      diff_time <- as.double.difftime(tm1 - tm, units = "secs")
      i <- i + 1
      print(c(nrow(train_idx_df), nrow(test_idx_df)))
    }, error = function(e) {
      i <- i + 1
      err <- as.character(e)
      write(x = err, file = "error_log.txt", append = T)
    })
  }
  
  metrics_df$file <- files[1:(i - 1)]
  dbWriteTable(mydb, "train_idx", train_idx_df)
  dbWriteTable(mydb, "test_idx", test_idx_df)
  
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
  legend("topright", legend = unique(loss_df$variable),
         fill = unique(loss_df$variable))
  dev.off()
  png("Report/mae.png", width = 554, height = 412)
  mae_df <- met_df[grep(met_df$variable, pattern =  ".*_mae$"), ]
  with(mae_df,
       plot(x = as.integer(factor(file)), y = value, col = variable, pch = 20,
            xlab = "File", xaxt = 'n'))
  legend("topright", legend = unique(mae_df$variable),
         fill = unique(mae_df$variable))
  dev.off()
  png("Report/mape.png", width = 554, height = 412)
  mape_df <- met_df[grep(met_df$variable, pattern =  ".*_mape$"), ]
  with(mape_df,
       plot(x = as.integer(factor(file)), y = value, col = variable, pch = 20,
            xlab = "File", xaxt = 'n'))
  legend("topright", legend = unique(mape_df$variable),
         fill = unique(mape_df$variable))
  dev.off()
  dbDisconnect(mydb)
  return(metrics_df)
}