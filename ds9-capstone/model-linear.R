# R version: 4.1.0
setwd("~/projects/data-science-course/ds9-capstone")

# suppress warnings
oldw <- getOption("warn")
options(warn = -1)

# clean memory
rm(edx, edx2)
rm(vector, df, test_index, p, p1, p2)

# environment
library(ggplot2)
library(caret)
library(tidyverse)

options(digits = 3)
subset_size = 500000

# read dataset from csv
if(!exists("train_set")) {train_set <- read_csv(file = "./dat/train.csv")}
if(!exists("test_set")) {test_set <- read_csv(file = "./dat/test.csv")}

# creates train subset for experiences with small size dataset
# df <- head(train_set, n=subset_size)
# rm(train_set)

# fit the model
# fit <- lm(rating ~ movieId + userId,
#           data = train_set)
# load(file="./mdl/fit.RData")

# predict the outcome
y_hat <- predict(fit, test_set) %>% as.numeric()

# rmse function definition
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

# check error metric
RMSE(test_set$rating, y_hat)

# save model
# save(fit, file="./mdl/fit.RData")

# restore warnings
options(warn = oldw)
