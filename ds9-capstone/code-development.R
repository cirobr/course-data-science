# R version: 4.1.0
print("job start")

# clean memory
print("clean memory")

# environment
print("setup environment")

# one-function libraries
# library(plyr)                 # used as plyr::round_any()
# library(varhandle)            # used as varhandle::unfactor()

# libraries
library(ggplot2)
library(lubridate)
library(tidyverse)
library(caret)

# global variables
options(digits = 3)
subsetSize = 5000

# read csv datasets
print("read csv datasets")
if(!exists("train_set")) {train_set <- read_csv(file = "./dat/train.csv") %>% as.tibble()}
if(!exists("test_set"))  {test_set  <- read_csv(file = "./dat/test.csv")  %>% as.tibble()}

# define error function
errRMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

# prepare datasets
df_train <- train_set %>% mutate(unbiasedRating = rating, .before = rating)
df_test  <- test_set  %>% mutate(unbiasedRating = rating, .before = rating)
# rm(train_set, test_set)

# unbias global average
muGlobalAvg <- mean(df_train$rating)

df_train <- df_train %>%
  mutate(unbiasedRating = unbiasedRating - muGlobalAvg)

df_test <- df_test %>%
  mutate(unbiasedRating = unbiasedRating - muGlobalAvg)

muUnbiased <- mean(df_train$unbiasedRating)
predicted <- rep(muUnbiased, nrow(test_set))

err <- errRMSE(df_test$unbiasedRating, predicted)
err

# unbias ratings per movie
df <- df_train %>%
  group_by(movieId) %>%
  summarize(avgRatingPerMovie = mean(rating))
df_train <- left_join(df_train, df)
df_test  <- left_join(df_test, df)

df_train$unbiasedRating <- df_train$unbiasedRating - df_train$avgRatingPerMovie
df_test$unbiasedRating  <- df_test$unbiasedRating  - df_test$avgRatingPerMovie
