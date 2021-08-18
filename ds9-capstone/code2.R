# R version: 4.1.0
print("job start")

# suppress warnings
# oldw <- getOption("warn")
# options(warn = -1)

# clean memory
print("clean memory")

# environment
print("setup environment")
# library(plyr)                 # used on plyr::round_any()
# library(varhandle)            # used on varhandle::unfactor()
library(ggplot2)
library(dplyr)
library(tidyverse)
library(caret)
library(randomForest)
library(nnet)
# library(doParallel)

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

# fit the model
print("fit naive average model")
mu <- mean(train_set$rating)

print("predict outcome")
predicted <- plyr::round_any(rep(mu, nrow(test_set)), 0.5)

print("calculate error")
err <- RMSE(test_set$rating, predicted)

rmse_results <- data.frame(model = "naiveAvg",
                           RMSE = err)


# fit the model
print("fit linear regression model")
# lm_fit <- lm(rating ~ userId + movieId,
#              data = train_set)

load(file="./mdl/lm_fit.RData")

print("predict outcome")
predicted <- plyr::round_any(predict(lm_fit, test_set), 0.5)
rm(lm_fit)

print("calculate error")
err <- RMSE(test_set$rating, predicted)

rmse_results <- bind_rows(rmse_results, data.frame(model = "linearReg",
                                                   RMSE = err))



# show RMSE results
rmse_results

# restore warnings
# options(warn = oldw)

print("job done")
