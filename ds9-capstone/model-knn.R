# R version: 4.1.0
setwd("~/projects/data-science-course/ds9-capstone")

# suppress warnings
#oldw <- getOption("warn")
#options(warn = -1)

subset_size = 50000
rating_to_predict = 5  # stored model only works with "5"

# clean memory
rm(edx, edx2)
rm(vector, df, test_index, p, p1, p2)

# environment
library(ggplot2)
library(caret)
library(tidyverse)

options(digits = 3)

# read dataset from csv
if(!exists("train_set")) {train_set <- read_csv(file = "./dat/train.csv")}
if(!exists("test_set")) {test_set <- read_csv(file = "./dat/test.csv")}

# creates train subset for experiences with small size dataset
train_subset <- head(train_set, n=subset_size)

# prepare datasets for prediction
train_subset$rating <- factor(train_subset$rating)

# clean memory
#rm(train_set, test_set)

# fit the model
fit_knn <- train_subset %>%
  train(rating ~ Drama + Comedy, 
        method = "knn",
        tuneGrid = data.frame(k = seq(1, 3, 2)),
        data = .)

# restore warnings
#options(warn = oldw)
