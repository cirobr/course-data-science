# R version: 4.0.3
setwd("~/projects/data-science-course/ds9-capstone")

# libraries
library(ggplot2)
library(caret)
library(tidyverse)

# read edx dataset from csv
#edx <- read_csv(file = "./dat/edx.csv")
head(edx)

# split edx in train and test sets (70 / 30)
set.seed(1, sample.kind = "Rounding")
y = edx$rating                                          # outcome to predict: movie rating for a given user
proportion = 0.20
test_index <- createDataPartition(y, times = 1, p = proportion, list = FALSE)
test_set <- edx %>% slice(test_index)
train_set <- edx %>% slice(-test_index)

# check for regularity of train / test splits
p1 <- train_set %>%
  group_by(rating) %>%
  summarize(qty = n()) %>%
  mutate(split = 'train_set')

p2 <- test_set %>%
  group_by(rating) %>%
  summarize(qty = n()) %>%
  mutate(split = 'test_set')

p <- bind_rows(p1, p2) %>% group_by(split)
p$rating <- as.factor(p$rating)
p %>% ggplot(aes(rating, qty, fill = split)) +
  geom_bar(stat="identity", position = "dodge") +
  ggtitle("Regularity of Train_set / Test_set split")
