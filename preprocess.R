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
  mse <- rep(0, 9999)
  for(i in 2:10000) {
    print(i)
    tser <- ts(data = temp$RESIDUAL_FLUX, frequency = i)
    dcomp <- decompose(tser)
    mse[i-1] <- mean(dcomp$random^2, na.rm = T)
  }
  plot(2:(length(mse)-1), mse)
  temp <- data.frame(read_delim(file, delim = " ", skip = 35, col_names = F),
                     stringsAsFactors = F)
  temp$X8 <- NULL
  temp <- data.frame(sapply(temp, as.numeric))
  cols <- read_lines("data/kplr001026957_q1_q16_tce_01_dvt_lc.tbl", skip = 31,
                     n_max = 1)
  cols <- strsplit(cols, "\\ {0,}\\|")[[1]][-c(1)]
  colnames(temp) <- cols
  temp$RESIDUAL_FLUX <- na.interpolation(temp$RESIDUAL_FLUX)
  fft_res <- fft(temp$RESIDUAL_FLUX)
  x <- Re(fft_res)
  y <- Im(fft_res)
  df <- data.frame(re = x, im = y)
  model <- keras_model_sequential()
  model %>%
    layer_dense(units = 15, activation = "tanh", input_shape = ncol(x)) %>%
    layer_dense(units = 10, activation = "tanh") %>%
    layer_dense(units = 15, activation = "tanh") %>%
    layer_dense(units = ncol(x_train))
  
  summary(model)
  plot(y ~ x, df)
  plot(x = temp$TIME, y = x)
  plot(x = temp$TIME, y = y)
  color <- ifelse(Mod(fft_res) > 3, "blue", "red")
  plot(RESIDUAL_FLUX ~ TIME, data = temp, col = color)
}