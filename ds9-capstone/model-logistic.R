# R version: 4.1.0
setwd("~/projects/data-science-course/ds9-capstone")

# suppress warnings
oldw <- getOption("warn")
options(warn = -1)

# clean memory
rm(edx, edx2)
rm(vector, df, test_index, p, p1, p2)

# environment
library(ggplot2)
library(caret)
library(tidyverse)

options(digits = 3)
subset_size = 500000

# read dataset from csv
train_set <- read_csv(file = "./dat/train.csv")
test_set <- read_csv(file = "./dat/test.csv")

# define rating to predict as the most common rating
df <- train_set %>% select(rating) %>% group_by(rating) %>% summarize(n = n())
df[which.max(df$n),]$rating
rating_to_predict = 4  # stored model developed for "4"

# prepare datasets for categorical analysis
train_set <- train_set %>%
  mutate(y = as.numeric(rating == rating_to_predict)) %>%
  select(-c(rating))

test_set <- test_set %>%
  mutate(y = as.numeric(rating == rating_to_predict)) %>%
  select(-c(rating))

# fit the model
# fit_log <- train_set %>% 
#   glm(y ~ userId + movieId,
#       data=.,
#       family=binomial)
# load(file="./mdl/fit_log.RData")

p_hat <- predict(fit_log, 
                 newdata = test_set, 
                 type = "response")
p_hat <- as.numeric(p_hat)

# rmse function definition
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

# cutoff analysis
f_err <- function(x){
  y_hat <- ifelse(p_hat >= x, 1, 0) %>% as.numeric()
  RMSE(test_set$y, y_hat)
}

# f_f1 <- function(x){
#   y_hat <- ifelse(p_hat >= x, 1, 0) %>% as.numeric() %>% factor(levels = factor(c(0, 1)))
#   F_meas(data=y_hat, reference=y_fac)
# }

cutoff <- seq(0.1, 0.9, 0.05)

err <- sapply(cutoff, f_err)
data.frame(cutoff, err) %>%
  ggplot(aes(cutoff, err)) +
  ggtitle("RMSE") +
  scale_x_continuous(breaks = cutoff) +
  geom_point() +
  geom_line() +
  geom_label(label = sprintf("%0.2f", round(err, digits = 2)), size = 2, nudge_y = -0.02)

# y_fac <- factor(test_set$y)
# f1 <- sapply(cutoff, f_f1)
# data.frame(cutoff, f1) %>%
#   ggplot(aes(cutoff, f1)) +
#   ggtitle("F1-score") +
#   scale_x_continuous(breaks = cutoff) +
#   geom_point() +
#   geom_line() +
#   geom_label(label = sprintf("%0.2f", round(f1, digits = 2)), size = 2, nudge_y = -0.05)

# cutoff = 0.20 evaluation
# y_hat <- ifelse(p_hat >= 0.3, 1, 0) %>% as.numeric() %>% factor(levels = factor(c(0, 1)))
# sensitivity(data = y_hat, reference = factor(test_set$y))
# specificity(data = y_hat, reference = factor(test_set$y))
# F_meas(data=y_hat, reference=factor(test_set$y))
# cm <- confusionMatrix(data=y_hat, reference=factor(test_set$y))
# cm$overall["Accuracy"]
# cm
 
# save model
# save(fit_log, file="./mdl/fit_log.RData")

# restore warnings
options(warn = oldw)
