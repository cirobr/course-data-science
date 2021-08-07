# R version: 4.1.0
setwd("~/projects/data-science-course/ds9-capstone")

# libraries
library(ggplot2)
library(caret)
library(tidyverse)

# read edx dataset from csv
if(!exists("train_set")) {train_set <- read_csv(file = "./dat/train.csv")}
if(!exists("test_set")) {test_set <- read_csv(file = "./dat/test.csv")}
train_set$rating <- as.factor(train_set$rating)
test_set$rating <- as.factor(test_set$rating)

# check for stratification of train / test split
p1 <- train_set %>%
  group_by(rating) %>%
  summarize(qty = n()) %>%
  mutate(split = 'train_set')

p2 <- test_set %>%
  group_by(rating) %>%
  summarize(qty = n()) %>%
  mutate(split = 'test_set')

p <- bind_rows(p1, p2) %>% group_by(split)
rm(p1, p2)
p %>% ggplot(aes(rating, qty, fill = split)) +
  geom_bar(stat="identity", position = "dodge") +
  ggtitle("Stratification of Test_set / Train_set split")

#train model
