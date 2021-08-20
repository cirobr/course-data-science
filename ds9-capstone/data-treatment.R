# R version: 4.1.0
print("job start")

# suppress warnings
oldw <- getOption("warn")
options(warn = -1)

# environment
print("setup environment")
# library(plyr)                 # used as plyr::round_any()
# library(varhandle)            # used as varhandle::unfactor()
library(ggplot2)
library(dplyr)
library(tidyverse)
library(caret)
library(randomForest)
library(nnet)
library(doParallel)

options(digits = 3)
subset_size = 5000

# paralell processing
# number_of_processor_cores = 3
# cl <- makePSOCKcluster(number_of_processor_cores)

# read csv datasets
print("read csv datasets")
if(!exists("train_set")) {train_set <- read_csv(file = "./dat/train.csv") %>% as.tibble()}
if(!exists("test_set"))  {test_set  <- read_csv(file = "./dat/test.csv") %>% as.tibble()}

# prepare datasets
print("prepare datasets")
df_train <- train_set %>% as.tibble()
df_test  <- test_set %>% as.tibble()

# creates small subset for experiments
df <- head(df_train, n=subset_size)

# no NAs on trainset / testset
# any(is.na(df_train))
# any(is.na(df_test))

