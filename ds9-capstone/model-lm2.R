# R version: 4.1.0
print("job start")

# suppress warnings
oldw <- getOption("warn")
options(warn = -1)

# clean memory
print("clean memory")
rm(edx, edx2)
rm(vector, df, test_index, p, p1, p2)

# environment
print("setup environment")
library(ggplot2)
library(plyr)
library(caret)
library(tidyverse)

options(digits = 3)
subset_size = 50000

# read csv datasets
print("read csv datasets")
if(!exists("train_set")) {train_set <- read_csv(file = "./dat/train.csv")}
if(!exists("test_set")) {test_set <- read_csv(file = "./dat/test.csv")}

# prepare datasets
print("prepare datasets")
train_set <- train_set %>% select(c(userId, movieId, rating))
test_set <- test_set %>% select(c(userId, movieId, rating))

# creates small size subset for experiences
# df <- head(train_set, n=subset_size)

# rmse function definition
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

# first approach - naive average
predicted <- rep(mean(train_set$rating), nrow(train_set))
predicted <- round_any(predicted, 0.5)
err <- RMSE(train_set$rating, predicted)
err
rmse_results <- data.frame(method = "naive",
                           RMSE = err)

# fit by userId
fit <- lm(rating ~ userId,
          data = train_set)
predicted <- predict(fit, test_set)
predicted <- round_any(predicted, 0.5)
err <- RMSE(train_set$rating, predicted)
err
rmse_results <- bind_rows(rmse_results, data.frame(method = "by user",
                                                   RMSE = err))

# fit by movieId
fit <- lm(rating ~ movieId,
          data = train_set)
predicted <- predict(fit, test_set)
predicted <- round_any(predicted, 0.5)
err <- RMSE(train_set$rating, predicted)
err
rmse_results <- bind_rows(rmse_results, data.frame(method = "by movie",
                                                   RMSE = err))

# fit by user + movie
fit <- lm(rating ~ userId + movieId,
          data = train_set)
predicted <- round(predict(fit, test_set), 1)
predicted <- round_any(predicted, 0.5)
err <- RMSE(train_set$rating, predicted)
err
rmse_results <- bind_rows(rmse_results, data.frame(method = "user and movie",
                                                   RMSE = err))





# restore warnings
options(warn = oldw)

print("job done")
