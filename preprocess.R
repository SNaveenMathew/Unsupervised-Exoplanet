library(readr)
library(imputeTS)
library(reticulate)
reticulate::use_condaenv("tf_gpu")
library(keras)

files <- list.files(path = "data/", pattern = "*.tbl", full.names = T)
system("sh counts.sh")
text <- system("sh counts_df.sh", intern = T)
text <- t(sapply(strsplit(text, "\t"), function(elem) c(elem[1], elem[2])))
text <- data.frame(text)
text$X2 <- as.numeric(text$X2)

for(file in files) {
  # mse <- rep(0, 9999)
  # for(i in 2:10000) {
  #   print(i)
  #   tser <- ts(data = temp$RESIDUAL_FLUX, frequency = i)
  #   dcomp <- decompose(tser)
  #   mse[i-1] <- mean(dcomp$random^2, na.rm = T)
  # }
  # plot(2:(length(mse)-1), mse)
  temp <- data.frame(read_delim(file, delim = " ", skip = 35, col_names = F),
                     stringsAsFactors = F)
  temp$X8 <- NULL
  temp <- data.frame(sapply(temp, as.numeric))
  cols <- read_lines(file, skip = 31, n_max = 1)
  cols <- strsplit(cols, "\\ {0,}\\|")[[1]][-c(1)]
  colnames(temp) <- cols
  # temp$RESIDUAL_FLUX <- na.interpolation(temp$RESIDUAL_FLUX)
  # fft_res <- fft(temp$RESIDUAL_FLUX)
  # x <- Re(fft_res)
  # y <- Im(fft_res)
  # df <- data.frame(re = x, im = y)
  tsteps <- 1
  batch_size   <- 40
  epochs       <- 300
  lag_setting <- 1
  model <- keras_model_sequential()
  
  model %>%
    layer_cudnn_lstm(units            = 50, 
               input_shape      = c(tsteps, 1), 
               batch_size       = batch_size,
               return_sequences = TRUE, 
               stateful         = F) %>% 
    layer_cudnn_lstm(units            = 50, 
               return_sequences = FALSE, 
               stateful         = F) %>% 
    layer_dense(units = 1)
  model %>% 
    compile(loss = 'mae', optimizer = 'adam')
  summary(model)
  lag_tbl <- temp %>%
    mutate(value_lag = lag(RESIDUAL_FLUX, n = lag_setting)) %>%
    filter(!is.na(value_lag))
  x_train_vec <- lag_tbl$value_lag
  x_train_arr <- array(data = x_train_vec, dim = c(length(x_train_vec), 1, 1))
  y_train_vec <- lag_tbl$value
  y_train_arr <- array(data = y_train_vec, dim = c(length(y_train_vec), 1))
  for (i in 1:epochs) {
    model %>% fit(x          = x_train_arr, 
                  y          = y_train_arr, 
                  batch_size = batch_size,
                  epochs     = 1, 
                  verbose    = 1, 
                  shuffle    = FALSE)
    
    model %>% reset_states()
    cat("Epoch: ", i)
    
  }
  
  plot(y ~ x, df)
  plot(x = temp$TIME, y = x)
  plot(x = temp$TIME, y = y)
  color <- ifelse(Mod(fft_res) > 3, "blue", "red")
  plot(RESIDUAL_FLUX ~ TIME, data = temp, col = color)
}