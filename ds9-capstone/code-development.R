# R version: 4.1.0
print("job start")

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

### unbias global average
muTrain <- mean(train_set$rating)
muTest  <- mean(test_set$rating)

unbiasedRatingTrain <- train_set$rating - muTrain
unbiasedRatingTest  <- train_set$rating - muTest

# predict
predicted <- mean(unbiasedRatingTrain)
predicted <- rep(predicted, nrow(test_set))

err <- errRMSE(unbiasedRatingTest, predicted)
err

### unbias ratings per movie
ratingsTrain <- train_set %>%
  group_by(movieId) %>%
  summarize(avgRatingPerMovie = mean(rating))
ratingsTest <- test_set %>%
  group_by(movieId) %>%
  summarize(avgRatingPerMovie = mean(rating))

df_train <- left_join(train_set, ratingsTrain)
df_test  <- left_join(test_set,  ratingsTest)

unbiasedRatingTrain <- df_train$rating - df_train$avgRatingPerMovie
unbiasedRatingTest  <- df_test$rating  - df_test$avgRatingPerMovie

# predict
predicted <- left_join(test_set, ratingsTrain)
predicted <- predicted$rating  - predicted$avgRatingPerMovie

err <- errRMSE(unbiasedRatingTest, predicted)
err
