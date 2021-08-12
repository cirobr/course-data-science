# R version: 4.1.0

# suppress warnings
# oldw <- getOption("warn")
# options(warn = -1)

# clean memory
rm(edx)
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
#df <- head(train_set, n=subset_size) %>% select(-c(rating))
df <- train_set %>% select(-c(rating))

# pca
res.pca <- prcomp(df, center = TRUE, scale. = TRUE)
s <- summary(res.pca)
ind <- max(which(s$importance[3,] < 0.95))
df_pca <- s$x[,1:ind] %>% as.data.frame()
df_pca <- cbind(train_set$rating, df_pca)
head(df_pca)
