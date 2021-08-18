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
library(nnet)
library(doParallel)

options(digits = 3)
subset_size = 50000

# paralell processing
number_of_processor_cores = 3
cl <- makePSOCKcluster(number_of_processor_cores)

# read csv datasets
print("read csv datasets")
if(!exists("train_set")) {train_set <- read_csv(file = "./dat/train.csv") %>% as.tibble()}
if(!exists("test_set"))  {test_set  <- read_csv(file = "./dat/test.csv") %>% as.tibble()}

# prepare datasets
print("prepare datasets")
df_train <- train_set %>% as.tibble()
df_test  <- test_set %>% as.tibble()

# transform predictors to integers and factor the outcome
ratingFactor <- as.factor(df_train$rating)
levelsRating <- levels(ratingFactor)
numberOfColumns <- ncol(df_train)
df_train <- df_train %>% 
  select(-c(rating)) %>% 
  mutate(across(1 : numberOfColumns-1, as.integer))
df_train <- bind_cols(rating=ratingFactor, df_train)

ratingFactor <- as.factor(df_test$rating)
df_test <- df_test %>% 
  select(-c(rating)) %>% 
  mutate(across(1 : numberOfColumns-1, as.integer))
df_test <- bind_cols(rating=ratingFactor, df_test)

# creates small subset for experiments
df <- head(df_train, n=subset_size)

# fit the model
print("fit randomForest model")
modelLookup("rf")

control <- trainControl(method = "cv", 
                        number = 10, 
                        p = .9,
                        verboseIter = TRUE)

maxMtry = floor((ncol(df_train) - 1) / 2)
gridSearch <- data.frame(mtry = seq(2, maxMtry, by = 1))

print("multi-core ON")
registerDoParallel(cl)

set.seed(1, sample.kind = "Rounding")
rf_bestfit <- df %>%
# rf_bestfit <- df_train %>%
  train(rating ~ .,
        method = "rf",
        ntree = 75,
        proximity = FALSE,
        data = .,
        tuneGrid = gridSearch,
        trControl = control
        )

stopCluster(cl)   # multi-core off

ggplot(rf_bestfit, highlight = TRUE)
rf_bestfit$bestTune
rf_bestfit$finalModel

# predict the outcome
print("predict the outcome")
predicted <- predict(rf_bestfit, df_test)
predictedNonFactor <- varhandle::unfactor(predicted)

# calculate error metrics
print("calculate error metrics")
err <- RMSE(test_set$rating, predictedNonFactor)
err

hist(predictedNonFactor)

# save model
# print("save the model")
# save(rf_bestfit, file="./mdl/rf_bestfit.RData")

# restore warnings
options(warn = oldw)
