# Note: Keras uses multiple cores. Therefore multiprocessing is not used

library(readr)
library(imputeTS)
library(reticulate)
# reticulate::use_condaenv("tf_gpu")
library(keras)
library(kerasR)
library(imputeTS)
library(reshape2)
library(dplyr)
library(docopt)
source("util.R")
source("pipeline.R")
doc <- "Usage: main.R [--PATH PATH]

--PATH=PATH       Relative path of folder containing data [default: data]"

opt <- docopt(doc)

set.seed(1)

run_pipeline(opt$PATH)
