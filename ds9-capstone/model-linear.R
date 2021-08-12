# R version: 4.1.0
print("job start")

# suppress warnings
oldw <- getOption("warn")
options(warn = -1)

# clean memory
print("clean memory")
rm(edx)
rm(vector, df, test_index, p, p1, p2)

# environment
print("setup environment")
library(ggplot2)
library(caret)
library(tidyverse)

options(digits = 3)
subset_size = 500000

# read csv datasets
print("read csv datasets")
train_set <- read_csv(file = "./dat/train.csv")
test_set <- read_csv(file = "./dat/test.csv")

# prepare datasets
print("prepare datasets")
train_set <- train_set %>% select(c(userId, movieId, rating))
test_set <- test_set %>% select(c(userId, movieId, rating))

# creates small size subset for experiences
# df <- head(train_set, n=subset_size)
# rm(train_set)

# fit the model
print("fit the model")
# fit <- lm(rating ~ .,
#           data = train_set)
load(file="./mdl/fit.RData")

# predict the outcome
print("predict the outcome")
y_hat <- predict(fit, test_set) %>% as.numeric()

# rmse function definition
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

# calculate error metrics
print("calculate error metrics")
RMSE(test_set$rating, y_hat)

# save model
# print("save the model")
# save(fit, file="./mdl/fit.RData")

# restore warnings
options(warn = oldw)

print("job done")
