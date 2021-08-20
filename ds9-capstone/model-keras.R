# R version: 4.1.0
print("job start")

# suppress warnings
# oldw <- getOption("warn")
# options(warn = -1)

# environment
print("setup environment")
# library(plyr)                 # used as plyr::round_any()
# library(varhandle)            # used as varhandle::unfactor()
library(ggplot2)
library(dplyr)
library(tidyverse)
library(caret)
library(BBmisc)
library(keras)

options(digits = 3)
subset_size = 5000

# paralell processing
# number_of_processor_cores = 3
# cl <- makePSOCKcluster(number_of_processor_cores)

# read csv datasets
print("read csv datasets")
if(!exists("train_set")) {train_set <- read_csv(file = "./dat/train.csv") %>% as_tibble()}
if(!exists("test_set"))  {test_set  <- read_csv(file = "./dat/test.csv") %>% as_tibble()}

# prepare datasets
print("prepare datasets")
df_train <- train_set %>% select(c(rating:year_stamp))
df_test  <- test_set  %>% select(c(rating:year_stamp))

# transform predictors to integers and factor the outcome
df_train$rating <- as.factor(df_train$rating)
df_test$rating  <- as.factor(df_test$rating)

# normalize predictors
fn <- function(vec){
  preProcess(vec, method = c("range"), rangeBounds = c(-1, 1))
}

df <- as.matrix(df_train[2:4])
X <- apply(df, 2, fn) %>% as_tibble()
