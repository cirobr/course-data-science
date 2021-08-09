# R version: 4.1.0
setwd("~/projects/data-science-course/ds9-capstone")
subset_size = 50000
rating_to_predict = 5

# clean memory
rm(edx, edx2)
rm(vector, df, test_index, p, p1, p2)

# environment
library(ggplot2)
library(caret)
library(tidyverse)

options(digits = 3)

#rm(train_set, test_set)
# read dataset from csv
if(!exists("train_set")) {train_set <- read_csv(file = "./dat/train.csv")}
#train_set$rating <- as.factor(train_set$rating)
if(!exists("test_set")) {test_set <- read_csv(file = "./dat/test.csv")}
#test_set$rating <- as.factor(test_set$rating)

# creates train subset for experiences with small size dataset
train_subset <- head(train_set, n=subset_size)

# prepare datasets for prediction of rate 5
m_train_set <- train_set %>%
  mutate(y = as.numeric(rating == rating_to_predict)) %>%
  select(-c(rating))

m_test_set <- test_set %>%
  mutate(y = as.numeric(rating == rating_to_predict)) %>%
  select(-c(rating))

m_train_subset <- train_subset %>%
  mutate(y = as.numeric(rating == rating_to_predict)) %>%
  select(-c(rating))

#rm(train_set, test_set, train_subset)

# fit the model
rm(train_set, test_set, train_subset)

fit_log <- m_train_set %>%
  glm(y ~ ., 
      data=., 
      family=binomial)

p_hat <- predict(fit_log, 
                 newdata = m_test_set, 
                 type = "response")

# cutoff analysis
f_acc <- function(x){
  y_hat <- ifelse(p_hat >= x, 1, 0) %>% as.numeric()
  mean(y_hat == y)
}

y = m_test_set$y
cutoff <- seq(0.1, 0.9, 0.1)
acc <- sapply(cutoff, f_acc)

data.frame(cutoff, acc) %>% 
  ggplot(aes(cutoff, acc)) + 
  geom_point() + 
  geom_line()

y_hat <- ifelse(p_hat >= 0.15, 1, 0) %>% as.numeric()
confusionMatrix(data=factor(y_hat, levels = factor(c(0, 1))), reference=factor(y, levels = factor(c(0, 1))))

# save model
save(fit_log, file="./mdl/fit_log.Rdata")
