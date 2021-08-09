# R version: 4.1.0
setwd("~/projects/data-science-course/ds9-capstone")
#subset_size = 50000
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
#train_subset <- head(train_set, n=subset_size)

# prepare datasets for prediction
m_train_set <- train_set %>%
  mutate(y = as.numeric(rating == rating_to_predict)) %>%
  select(-c(rating))

m_test_set <- test_set %>%
  mutate(y = as.numeric(rating == rating_to_predict)) %>%
  select(-c(rating))

# m_train_subset <- train_subset %>%
#   mutate(y = as.numeric(rating == rating_to_predict)) %>%
#   select(-c(rating))

# clean memory
rm(train_set, test_set, train_subset)

# fit the model
# fit_log <- m_train_set %>% glm(y ~ .,
#                                data=.,
#                                family=binomial)
# load(file="./mdl/fit_log.RData")

p_hat <- predict(fit_log, 
                 newdata = m_test_set, 
                 type = "response")

# cutoff analysis
f_acc <- function(x){
  y_hat <- ifelse(p_hat >= x, 1, 0) %>% as.numeric()
  mean(y_hat == y)
}

f_f1 <- function(x){
  y_hat <- ifelse(p_hat >= x, 1, 0) %>% as.numeric() %>% factor(levels = factor(c(0, 1)))
  F_meas(data=y_hat, reference=y_fac)
}

cutoff <- seq(0.1, 0.9, 0.05)

y = m_test_set$y
acc <- sapply(cutoff, f_acc)

data.frame(cutoff, acc) %>% 
  ggplot(aes(cutoff, acc)) + 
  ggtitle("Accuracy") +
  scale_x_continuous(breaks = cutoff) +
  geom_point() + 
  geom_line() +
  geom_label(label = sprintf("%0.2f", round(acc, digits = 2)), size = 2, nudge_y = -0.05)

y_fac <- y %>% factor()
f1 <- sapply(cutoff, f_f1)

data.frame(cutoff, f1) %>% 
  ggplot(aes(cutoff, f1)) + 
  ggtitle("F1-score") +
  scale_x_continuous(breaks = cutoff) +
  geom_point() + 
  geom_line() +
  geom_label(label = sprintf("%0.2f", round(f1, digits = 2)), size = 2, nudge_y = -0.05)

# cutoff = 0.5 evaluation  
y_hat <- ifelse(p_hat >= 0.5, 1, 0) %>% as.numeric() %>% factor(levels = factor(c(0, 1)))
sensitivity(data = y_hat, reference = y_fac)
specificity(data = y_hat, reference = y_fac)
F_meas(data=y_hat, reference=y_fac)
confusionMatrix(data=y_hat, reference=y_fac)

# save model
# save(fit_log, file="./mdl/fit_log.RData")
