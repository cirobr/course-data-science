# R version: 4.1.0
print("job start")

# suppress warnings
oldw <- getOption("warn")
options(warn = -1)

# environment
print("setup environment")
# library(plyr)                 # used as plyr::round_any()
# library(varhandle)            # used as varhandle::unfactor()
library(ggplot2)
library(dplyr)
library(tidyverse)
library(caret)

library(randomForest)
library(doParallel)

options(digits = 3)
subset_size = 5000

# paralell processing
# number_of_processor_cores = 6
# cl <- makePSOCKcluster(number_of_processor_cores)

# define error function
errRMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

# read csv datasets
print("read csv datasets")
if(!exists("train_set")) {train_set <- read_csv(file = "./dat/train.csv") %>% as_tibble()}
if(!exists("test_set"))  {test_set  <- read_csv(file = "./dat/test.csv") %>% as_tibble()}

# prepare datasets
print("prepare datasets")
df_train <- train_set
df_test  <- test_set

# scale predictors and factor the outcome
outcomeFactors <- c("0.5", "1", "1.5", "2", "2.5", "3", "3.5", "4", "4.5", "5")

df_train$rating <- factor(df_train$rating, levels = outcomeFactors)
df_train[-1] = scale(df_train[-1])

df_test$rating <- factor(df_test$rating, levels = outcomeFactors)
df_test[-1] = scale(df_test[-1])

# creates small subset for experiments
df <- head(df_train, n=subset_size)

# fit the model
print("fit randomForest model")
modelLookup("rf")

control <- trainControl(method = "cv",
                        number = 10,
                        p = .9,
                        verboseIter = TRUE)

maxMtry <- ceiling((ncol(df_train) - 1) / 2)
gridSearch <- expand.grid(mtry  = seq(3, maxMtry, by = 1))

# print("multi-core ON")
# registerDoParallel(cl)

set.seed(1, sample.kind = "Rounding")
rf_fit <- df_train %>%
  train(rating ~ .,
        method = "rf",
        data = .,
        ntree = 100,
        proximity = FALSE,
        tuneGrid = gridSearch,
        trControl = control
        )

# stopCluster(cl)   # multi-core off

# load pre-built model to save execution time
# load(file="./mdl/rf_fit.RData")

ggplot(rf_fit, highlight = TRUE)
rf_fit$bestTune
rf_fit$finalModel

# predict the outcome
print("predict the outcome")
predicted <- predict(rf_fit, df_test)
predictedNonFactor <- varhandle::unfactor(predicted)

# calculate error metrics
print("calculate error metrics")
err <- errRMSE(test_set$rating, predictedNonFactor)
err
hist(predictedNonFactor)

# save model
print("save model")
save(rf_fit, file="./mdl/rf_fit.RData")

# clean memory
rm(rf_fit, df, df_train, df_test)

# restore warnings
options(warn = oldw)
