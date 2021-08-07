# R version: 4.1.0
setwd("~/projects/data-science-course/ds9-capstone")

# libraries
library(ggplot2)
library(caret)
library(tidyverse)

# read dataset from csv
if(exists("test_set")) {rm(test_set)}
if(!exists("train_set")) {train_set <- read_csv(file = "./dat/train.csv")}
train_set$rating <- as.factor(train_set$rating)

# train model
