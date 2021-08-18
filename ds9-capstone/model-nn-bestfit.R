# R version: 4.1.0
print("job start")

# suppress warnings
# oldw <- getOption("warn")
# options(warn = -1)

# clean memory
print("clean memory")

# environment
print("setup environment")
library(ggplot2)
# library(plyr)
library(dplyr)
library(tidyverse)
library(varhandle)            # unfactor() function
library(caret)
library(nnet)
library(randomForest)
library(doParallel)

options(digits = 3)
subset_size = 500000
number_of_iterations <- 200
number_of_processor_cores = 2

# read csv datasets
print("read csv datasets")
if(!exists("train_set")) {train_set <- read_csv(file = "./dat/train.csv")}
if(!exists("test_set"))  {test_set  <- read_csv(file = "./dat/test.csv")}

# define error function
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

# prepare datasets
print("prepare datasets")
df_train <- train_set %>% as_tibble()
df_test  <- test_set %>% as_tibble()

# remove rows with small variance
# nzv <- nearZeroVar(df_train)
nzv <- c(16, 17, 19, 21, 22, 23)   # remover após testes
df_train <- df_train[,-nzv]
df_test <- df_test[,-nzv]

# factor the outcome
df_train$rating <- as.factor(df_train$rating)
df_test$rating <- as.factor(df_test$rating)

# create small subset for code testing
df <- head(df_train, n=subset_size)

# fit the model
print("fit nnet model")
modelLookup("nnet")

control <- trainControl(method = "cv", number = 10, p = .9)

gridSearch <- expand.grid(size = seq(from = 10, to = 20, by = 10),
                          decay = seq(from = 0.0, to = 0.1, by = 0.05))

print("multi-core ON")
cl <- makePSOCKcluster(number_of_processor_cores)
registerDoParallel(cl)

set.seed(1, sample.kind = "Rounding")
# nnet_bestfit <- df_train %>%
nnet_bestfit <- df %>%
  train(rating ~ .,
        method = "nnet",
        maxit = number_of_iterations,
        data = .,
        tuneGrid = gridSearch,
        trControl = control
        )

stopCluster(cl)   # multi-core off

ggplot(nnet_bestfit, highlight = TRUE)
nnet_bestfit$bestTune
nnet_bestfit$finalModel

# load(file="./mdl/nnet_bestfit.RData")

# predict the outcome
print("predict the outcome")
predicted <- predict(nnet_bestfit, df_test)
predictedNonFactor <- unfactor(predicted)

# calculate error metrics
print("calculate error metrics")
err <- RMSE(test_set$rating, predictedNonFactor)
err

hist(predictedNonFactor)

# save model
# print("save the model")
# save(nnet_bestfit, file="./mdl/nnet_bestfit.RData")

# restore warnings
# options(warn = oldw)

print("job done")
