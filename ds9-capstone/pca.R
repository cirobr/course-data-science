# R version: 4.1.0
setwd("~/projects/data-science-course/ds9-capstone")

# suppress warnings
# oldw <- getOption("warn")
# options(warn = -1)

# clean memory
rm(edx, edx2)
rm(vector, df, test_index, p, p1, p2)

# environment
library(ggplot2)
library(caret)
library(tidyverse)
library(nnet)
library(stats)

options(digits = 3)
subset_size = 500000
#number_of_iterations <- 100

# read datasets from csv
train_set <- read_csv(file = "./dat/train.csv")
#train_set$rating <- as.factor(train_set$rating)

# creates small subset for experiments
df <- head(train_set, n=subset_size) %>% select(-c(rating, userId, movieId))

# pca
res.pca <- prcomp(df, scale = FALSE)
fviz_eig(res.pca)
