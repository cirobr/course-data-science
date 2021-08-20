# R version: 4.1.0
print("job start")

# suppress warnings
# oldw <- getOption("warn")
# options(warn = -1)

# clean memory
print("clean memory")

# environment
print("setup environment")
# library(plyr)                 # used as plyr::round_any()
# library(varhandle)            # used as varhandle::unfactor()
library(ggplot2)
library(dplyr)
library(tidyverse)
library(caret)
library(randomForest)
library(nnet)
library(doParallel)

options(digits = 3)
subset_size = 5000
number_of_iterations <- 200

# paralell processing
number_of_processor_cores = 3
cl <- makePSOCKcluster(number_of_processor_cores)

# read csv datasets
print("read csv datasets")
if(!exists("train_set")) {train_set <- read_csv(file = "./dat/train.csv") %>% as.tibble()}
if(!exists("test_set"))  {test_set  <- read_csv(file = "./dat/test.csv") %>% as.tibble()}

# prepare datasets
print("prepare datasets")
df_train <- train_set %>% as.tibble() %>% select(c(userId, movieId, rating))
df_test  <- test_set %>% as.tibble()  %>% select(c(userId, movieId, rating))

df_train$rating <- as.factor(df_train$rating)
df_test$rating <- as.factor(df_test$rating)

# create small subset for code testing
df <- head(df_train, n=subset_size)

# fit the model
print("fit nnet model")
modelLookup("nnet")

control <- trainControl(method = "cv", 
                        number = 10, 
                        p = .9,
                        verboseIter = TRUE)

gridSearch <- expand.grid(size = seq(from = 10, to = 50, by = 10),
                          decay = seq(from = 0.0, to = 0.0001, by = 0.00002))

print("multi-core ON")
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

# predict the outcome
print("predict the outcome")
predicted <- predict(nnet_bestfit, df_test)
predictedNonFactor <- varhandle::unfactor(predicted)

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
