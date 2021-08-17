# R version: 4.1.0
print("job start")

# suppress warnings
oldw <- getOption("warn")
options(warn = -1)

# clean memory
print("clean memory")
rm(edx, edx2)
rm(vector, df, test_index, p, p1, p2)

# environment
print("setup environment")
library(ggplot2)
library(caret)
library(tidyverse)

options(digits = 3)
subset_size = 50000

# read csv datasets
print("read csv datasets")
train_set <- read_csv(file = "./dat/train.csv")
test_set <- read_csv(file = "./dat/test.csv")

# define rating to predict as the most common rating
df <- train_set %>% select(rating) %>% group_by(rating) %>% summarize(n = n())
df[which.max(df$n),]$rating
rating_to_predict = 4  # stored model developed for "4"

# prepare datasets
print("prepare datasets")
train_set <- train_set %>% select(c(userId, movieId, rating)) %>%
  mutate(y = as.numeric(rating == rating_to_predict)) %>%
  select(-c(rating))
test_set <- test_set %>% select(c(userId, movieId, rating)) %>%
  mutate(y = as.numeric(rating == rating_to_predict)) %>%
  select(-c(rating))

# fit the model
print("fit the model")
# logit_fit <- glm(y ~ .,
#                  data = train_set,
#                  family=binomial)

load(file="./mdl/logit_fit.RData")

# predict the outcome
print("predict the outcome")
p_hat <- predict(logit_fit, 
                 newdata = test_set, 
                 type = "response")
p_hat <- as.numeric(p_hat)

# rmse function definition
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

# calculate error metrics
print("calculate error metrics")
f_err <- function(x){
  y_hat <- ifelse(p_hat >= x, 1, 0) %>% as.numeric()
  RMSE(test_set$y, y_hat)
}

cutoff <- seq(0.05, 0.95, 0.05)
err <- sapply(cutoff, f_err)
data.frame(cutoff, err) %>%
  ggplot(aes(cutoff, err)) +
  ggtitle("RMSE") +
  scale_x_continuous(breaks = cutoff) +
  geom_point() +
  geom_line() +
  geom_label(label = sprintf("%0.2f", round(err, digits = 2)), size = 2, nudge_y = -0.02)

# confusion matrix analysis
y_hat <- ifelse(p_hat >= 0.50, 1, 0) %>% as.numeric()
cm <- confusionMatrix(factor(y_hat, levels = c(0, 1)), factor(test_set$y, levels = c(0, 1)))
cm

# save model
# print("save the model")
# save(logit_fit, file="./mdl/logit_fit.RData")

# restore warnings
options(warn = oldw)
