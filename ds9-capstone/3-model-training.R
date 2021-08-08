# R version: 4.1.0
setwd("~/projects/data-science-course/ds9-capstone")

# libraries
library(ggplot2)
library(caret)
library(tidyverse)

# read dataset from csv
rm(train_set, test_set)
if(!exists("train_subset")) {train_subset <- read_csv(file = "./dat/subset.csv")}
train_subset$rating <- as.factor(train_subset$rating)

# train model
