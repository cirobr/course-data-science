# R version: 4.1.0

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
rating_to_predict = 4

# read csv datasets
print("read csv datasets")

train_set <- read_csv(file = "./dat/train.csv")
test_set <- read_csv(file = "./dat/test.csv")

# prepare datasets
print("prepare datasets")

train_set <- train_set %>% select(c(userId, movieId, rating)) %>%
  mutate(y = as.numeric(rating == rating_to_predict)) %>%
  select(-c(rating))
test_set <- test_set %>% select(c(userId, movieId, rating)) %>%
  mutate(y = as.numeric(rating == rating_to_predict)) %>%
  select(-c(rating))

train_set$y <- as.factor(train_set$y)
test_set$y <- as.factor(test_set$y)

# creates small subset for experiments
df <- head(train_set, n=subset_size)

# fit the model
print("fit the model")
knn_fit <- knn3(y ~ .,
                #data = df,
                data = train_set,
                k = 5
                )

# knn_fit <- train(y ~ .,
#                  method = "knn",
#                  tuneGrid = data.frame(k = 1),
#                  #data = df
#                  data = train_set
# )

# load(file="./mdl/knn_fit.RData")

# predict the outcome
print("predict the outcome")
y_hat <- predict(knn_fit, test_set, type = "class")
#y_hat <- predict(knn_fit, test_set, type = "raw")

# rmse function definition
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

# calculate error metrics
print("calculate error metrics")
RMSE(as.numeric(test_set$y), as.numeric(y_hat))

# confusion matrix analysis
cm <- confusionMatrix(y_hat, test_set$y)
cm

# save model
print("save the model")
save(knn_fit, file="./mdl/knn_fit.RData")

# restore warnings
options(warn = oldw)
