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

library(kernlab)                # library for svm
library(doParallel)

options(digits = 3)
subset_size = 5000

# define error function
errRMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

# paralell processing
number_of_processor_cores = 10
cl <- makePSOCKcluster(number_of_processor_cores)

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
print("fit svm model")
# modelLookup("svmLinear")

control <- trainControl(method = "cv",
                        number = 10,
                        p = .9,
                        verboseIter = TRUE)

gridSearch <- expand.grid(C = seq(0.1, 0.9, by=0.2),
                          c(1, 2, 3),
                          scale = 1
)

print("multi-core ON")
registerDoParallel(cl)

set.seed(1, sample.kind = "Rounding")
# svm_bestfit <- df %>%
#   train(rating ~ .,
#         method = "svmPoly",
#         data = .,
#         tuneGrid = gridSearch,
#         trControl = control
#   )

stopCluster(cl)   # multi-core off

# load pre-built model to save execution time
load(file="./mdl/svm_bestfit.RData")

# evaluate model
ggplot(svm_bestfit, highlight = TRUE)
svm_bestfit$bestTune
svm_bestfit$finalModel

# predict the outcome
print("predict the outcome")
predicted <- predict(svm_bestfit, df_test)
predictedNonFactor <- varhandle::unfactor(predicted)

# calculate error metrics
print("calculate error metrics")
err <- errRMSE(test_set$rating, predictedNonFactor)
err
hist(predictedNonFactor)

# save model
print("save model")
# save(svm_bestfit, file="./mdl/svm_bestfit.RData")

# clean memory
rm(svm_bestfit, df, df_train, df_test)

# restore warnings
options(warn = oldw)
