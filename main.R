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
source("util.R")
source("pipeline.R")

set.seed(1)

run_pipeline()
