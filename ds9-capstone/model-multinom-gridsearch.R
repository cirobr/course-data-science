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
number_of_iterations <- 250

# paralell processing
number_of_processor_cores = 3
cl <- makePSOCKcluster(number_of_processor_cores)

# read csv datasets
print("read csv datasets")
if(!exists("train_set")) {train_set <- read_csv(file = "./dat/train.csv") %>% as.tibble()}
if(!exists("test_set"))  {test_set  <- read_csv(file = "./dat/test.csv") %>% as.tibble()}

# prepare datasets
print("prepare datasets")
df_train <- train_set # %>% select(c(userId, movieId, rating))
df_test  <- test_set  # %>% select(c(userId, movieId, rating))

df_train$rating <- as.factor(df_train$rating)
df_test$rating  <- as.factor(df_test$rating)

# creates small subset for experiments
df <- head(df_train, n=subset_size)

# fit the model
print("fit penalized multinomial regression model")
modelLookup("multinom")

control <- trainControl(method = "cv", 
                        number = 10, 
                        p = .9,
                        verboseIter = TRUE)

gridSearch <- data.frame(decay = seq(0, 0.1, 0.05))

print("multi-core ON")
registerDoParallel(cl)

set.seed(1, sample.kind = "Rounding")
multinom_bestfit <- df %>%
#multinom_bestfit <- df_train %>%
  train(rating ~ ., 
        method = "multinom", 
        data = .,
        maxit = number_of_iterations,
        tuneGrid = gridSearch,
        trControl = control
        )

stopCluster(cl)   # multi-core off

ggplot(multinom_bestfit, highlight = TRUE)
multinom_bestfit$bestTune
multinom_bestfit$finalModel

# predict the outcome
print("predict outcome")
predicted <- predict(multinom_bestfit, df_test)
predictedNonFactor <- varhandle::unfactor(predicted)

# calculate error metrics
print("calculate error")
err <- RMSE(test_set$rating, predictedNonFactor)
err

hist(predictedNonFactor)

# save model
# print("save the model")
# save(multinom_bestfit, file="./mdl/multinom_bestfit.RData")

# restore warnings
options(warn = oldw)

print("job done")
