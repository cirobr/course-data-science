# R version: 4.1.0
print("job start")

# suppress warnings
oldw <- getOption("warn")
options(warn = -1)

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

# read csv datasets
print("read csv datasets")
if(!exists("train_set")) {train_set <- read_csv(file = "./dat/train.csv") %>% as.tibble()}
if(!exists("test_set"))  {test_set  <- read_csv(file = "./dat/test.csv")  %>% as.tibble()}

# define error function
errRMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

# fit the model
print("fit naive average model")
mu <- mean(train_set$rating)

print("predict outcome")
predicted <- plyr::round_any(rep(mu, nrow(test_set)), 0.5)

print("calculate error")
err <- errRMSE(test_set$rating, predicted)

rmse_results <- data.frame(model = "naiveAvg",
                           error = err)


# fit the model
print("fit linear regression model")
modelLookup("lm")

# lm_fit <- lm(rating ~ userId + movieId,
#              data = train_set)

# load pre-built model to save execution time
load(file="./mdl/lm_fit.RData")

print("predict outcome")
predicted <- plyr::round_any(predict(lm_fit, test_set), 0.5) %>% as.numeric()

print("calculate error")
err <- errRMSE(test_set$rating, predicted)

rmse_results <- bind_rows(rmse_results, data.frame(model = "linearReg",
                                                   error = err))






# show RMSE results
rmse_results

# cleanup memory
rm(lm_fit, predicted)

# restore warnings
options(warn = oldw)

print("job done")
