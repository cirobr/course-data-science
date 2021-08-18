# R version: 4.1.0
print("job start")

# suppress warnings
oldw <- getOption("warn")
options(warn = -1)

# clean memory
print("clean memory")
rm(edx, edx2)
rm(vector, df, test_index, p, p1, p2)

rm(lm_fit, multinom_fit, multinom_bestfit, df_train, df_test)

# environment
print("setup environment")
library(ggplot2)
library(plyr)
library(dplyr)
library(tidyverse)
library(caret)
library(nnet)
library(varhandle)
library(randomForest)

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

rating <- as.factor(df_train$rating)
lev <- levels(rating)
df_train <- df_train %>% select(-c(rating)) %>% mutate(across(userId:NoGenre, as.integer))
df_train <- bind_cols(rating=rating, df_train)
# head(df_train)

rating <- as.factor(df_test$rating)
df_test <- df_test %>% select(-c(rating)) %>% mutate(across(userId:NoGenre, as.integer))
df_test <- bind_cols(rating=rating, df_test)
# head(df_test)

# creates small subset for experiments
df <- head(train_set, n=subset_size)

# fit the model
print("fit the model")

# rf_fit <- df_train %>%
rf_fit <- df %>%
  randomForest(rating ~ .,
               ntree = 50,
               proximity = FALSE,
               data = .)

# load(file="./mdl/rf_fit.RData")

# predict the outcome
print("predict the outcome")
predicted <- round_any(predict(rf_fit, df_test, type = "class"), 0.5)

# calculate error metrics
print("calculate error metrics")
err <- RMSE(test_set$rating, predicted)
err

hist(predicted)

# save model
print("save the model")
save(rf_fit, file="./mdl/rf_fit.RData")

# restore warnings
options(warn = oldw)
