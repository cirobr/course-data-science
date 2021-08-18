# R version: 4.1.0
print("job start")

# suppress warnings
# oldw <- getOption("warn")
# options(warn = -1)

# environment
print("setup environment")
library(ggplot2)
#library(plyr)
library(dplyr)
library(tidyverse)
library(caret)
library(nnet)
#library(varhandle)
#library(randomForest)

options(digits = 3)
subset_size = 5000

# read csv datasets
print("read csv datasets")
if(!exists("train_set")) {train_set <- read_csv(file = "./dat/train.csv")}
if(!exists("test_set"))  {test_set  <- read_csv(file = "./dat/test.csv")}

# define error function
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

# prepare datasets
print("prepare datasets")
df_train <- train_set %>% as.tibble()
df_test  <- test_set %>% as.tibble()

# remove rows with small variance
nzv <- nearZeroVar(df_train)
df_train <- df_train[,-nzv]
df_test <- df_test[,-nzv]

# create small subset for code testing
df <- head(train_set, n=subset_size)
df$rating <- as.factor(df$rating)

stop()
# fit the model
print("fit the model")

# nn_fit <- df_train %>%
nn_fit <- df %>%
  nnet(rating ~ .,
       size = 100,
       decay = 0.1,
       data = .)

# predict the outcome
print("predict the outcome")
predicted <- predict(nn_fit, df_test)
hist(predicted)
