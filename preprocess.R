library(readr)
library(imputeTS)

files <- list.files(path = "data/", pattern = "*.tbl", full.names = T)

for(file in files) {
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
  plot(y ~ x, df)
  plot(x = temp$TIME, y = x)
  plot(x = temp$TIME, y = y)
}