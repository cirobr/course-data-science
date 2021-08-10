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

options(digits = 3)
subset_size = 500000
number_of_iterations <- 100

# read dataset from csv
train_set <- read_csv(file = "./dat/train.csv")
test_set <- read_csv(file = "./dat/test.csv")

# prepare datasets for categorical analysis
train_set$rating <- as.factor(train_set$rating)
test_set$rating <- as.factor(test_set$rating)

# creates small subset for experiments
# df <- head(train_set, n=subset_size)

# fit multi-class logistic regression
# fit_multinom <- df %>% 
#   multinom(rating ~ userId + movieId,
#            data = ., 
#            maxit=number_of_iterations)
load(file="./mdl/fit_multinom.RData")

p_hat <- fitted(fit_multinom, test_set)
y_hat <- colnames(p_hat)[max.col(p_hat)]
y_db <- y_hat %>% tibble()
colnames(y_db) <- "rating"
y_db %>% 
  group_by(rating) %>% 
  summarise(count = n()) %>% 
  spread(rating, count) %>% 
  prop.table() %>% 
  gather(rating, proportion) %>%
  ggplot(aes(rating, proportion)) +
  geom_bar(stat="identity", position = "dodge") +
  ggtitle("Proportion of predictions") +
  geom_label(label = proportion)
  #sprintf("%0.2f", round(proportion, digits = 2)), size = 2, nudge_y = -0.02)


  
# save model
# save(fit_multinom, file="./mdl/fit_multinom.RData")

# restore warnings
# options(warn = oldw)
