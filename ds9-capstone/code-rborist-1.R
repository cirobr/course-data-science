# R version: 4.1.0
print("job start")

# environment
print("setup environment")

# libraries
library(ggplot2)
library(lubridate)
library(tidyverse)
library(caret)
library(foreach)
library(Rborist)                # parallel computing

# global variables
options(digits = 3)
subsetSize = 5000

# read csv datasets
print("read csv datasets")
if(!exists("train_set")) {train_set <- read_csv(file = "./dat/train.csv") %>% as.tibble()}
if(!exists("test_set"))  {test_set  <- read_csv(file = "./dat/test.csv")  %>% as.tibble()}

# prepare datasets
df_train <- train_set %>%
  left_join(dfBiasMovie) %>%
  left_join(dfBiasUser) %>%
  mutate(deltaRating = (rating - mu - biasMovie - biasUser), 
         .before = rating) %>%
  select(-c(rating, 
            userId, 
            movieId, 
            timestamp, 
            timestampYear, 
            yearOfRelease,
            biasMovie,
            biasUser))

rm(train_set)

# remove predictors with small variance
nzv <- df_train %>% 
  select(-deltaRating) %>%
  nearZeroVar(foreach = TRUE, allowParallel = TRUE)

removedPredictors <- colnames(df_train[,nzv])
df_train <- df_train %>% select(-removedPredictors)

# scale predictors and factor the outcome
df_train[-1]    <- scale(df_train[-1])

# fit the model
print("fit Rborist model")
modelLookup("Rborist")

control <- trainControl(method = "cv",
                        number = 10,
                        p = .9,
                        verboseIter = TRUE)

numberOfPredictors <- ncol(df_train) - 1
maxPredictors <- floor(numberOfPredictors / 2)
gridSearch <- expand.grid(predFixed = seq(2, maxPredictors, by = 2),
                          minNode   = seq(500, 1000, 250)
)

set.seed(1, sample.kind = "Rounding")
rborist_fit <- df_train %>%
  train(deltaRating ~ .,
        method = "Rborist",
        data = .,
        nTree=100,
        tuneGrid = gridSearch,
        trControl = control
  )

# save model
# print("save model")
save(rborist_fit, file="./mdl/rborist_fit-1.RData")

# load pre-built model to save execution time
# load(file="./mdl/rborist_fit.RData")

ggplot(rborist_fit, highlight = TRUE)
rborist_fit$bestTune
rborist_fit$finalModel

# prepare test set for prediction
df_test <- test_set %>%
  left_join(dfBiasMovie) %>%
  left_join(dfBiasUser) %>%
  mutate(deltaRating = (rating - mu - biasMovie - biasUser), 
         .before = rating) %>%
  select(-c(rating, 
            userId, 
            movieId, 
            timestamp, 
            timestampYear, 
            yearOfRelease,
            biasMovie,
            biasUser)) %>%
  select(-removedPredictors)

df_test[-1]    <- scale(df_test[-1])

# predict the outcome
print("predict the outcome")
p <- predict(rborist_fit, df_test)

df <- test_set %>%
  select(rating, userId, movieId) %>%
  left_join(dfBiasMovie) %>%
  left_join(dfBiasUser) %>%
  mutate(predicted = mu + biasMovie + biasUser + p)

# calculate error metrics
print("calculate error metrics")
err <- errRMSE(test_set$rating, df$predicted)
err

