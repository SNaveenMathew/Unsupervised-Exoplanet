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
  temp <- data.frame(read_delim(file, delim = " ", skip = 35, col_names = F),
                     stringsAsFactors = F)
  temp$X8 <- NULL
  temp <- data.frame(sapply(temp, as.numeric))
  cols <- read_lines(file, skip = 31, n_max = 1)
  cols <- strsplit(cols, "\\ {0,}\\|")[[1]][-c(1)]
  colnames(temp) <- cols
}