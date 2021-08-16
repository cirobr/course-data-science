# R version: 4.1.0
print("job start")

# suppress warnings
oldw <- getOption("warn")
options(warn = -1)

# clean memory
print("clean memory")
rm(edx, edx2)
rm(vector, df, test_index, p, p1, p2)

rm(lm_fit)

# environment
print("setup environment")
library(ggplot2)
library(plyr)
library(tidyverse)
library(caret)
library(nnet)
library(varhandle)
library(randomForest)

options(digits = 3)
subset_size = 5000
threshold <- 0.05

# environment
print("setup environment")
number_of_iterations <- 250

# read csv datasets
print("read csv datasets")
if(!exists("train_set")) {train_set <- read_csv(file = "./dat/train.csv")}
if(!exists("test_set"))  {test_set  <- read_csv(file = "./dat/test.csv")}

# prepare datasets
print("prepare datasets")
df_train <- train_set # %>% select(c(userId, movieId, rating))
df_test  <- test_set  # %>% select(c(userId, movieId, rating))

df_train$rating <- as.factor(df_train$rating)
df_test$rating  <- as.factor(df_test$rating)

# find logical predictors with frequency higher than threshold
nr <- nrow(train_set)

threshold_genres <- train_set %>% 
  select(-c(userId, movieId, rating)) %>%
  colSums() / nr %>%
  sort(decreasing = TRUE)
threshold_genres <- tibble(genres=names(threshold_genres), p=threshold_genres) %>%
  filter(p > threshold) %>% 
  select(genres)
threshold_genres <- threshold_genres$genres

frequent_predictors <- c(c("userId", "movieId", "rating"), threshold_genres)

# select frequent predictors
df_train <- df_train %>% select(frequent_predictors)
df_test  <- df_test  %>% select(frequent_predictors)

# creates small subset for experiments
df <- head(df_train, n=subset_size)

# fit the model
print("fit multinomial linear regression model")
modelLookup("multinom")

control <- trainControl(method = "cv", number = 10, p = .9)     # 10-fold cv

multinom_bestfit <- df_train %>%
  train(rating ~ ., 
        method = "multinom", 
        data = .,
        maxit = number_of_iterations,
        tuneGrid = data.frame(decay = seq(0, 0.1, 0.05)),  # default decay = 0
        trControl = control
        )

ggplot(multinom_bestfit, highlight = TRUE)
multinom_bestfit$bestTune
multinom_bestfit$finalModel

# multinom_bestfit <- df %>%
#   multinom(rating ~ .,
#            data = .,
#            maxit=number_of_iterations)

# load(file="./mdl/multinom_fit.RData")

# predict the outcome
print("predict outcome")
predicted <- predict(multinom_bestfit, df_test)

# calculate error metrics
print("calculate error")
err <- RMSE(test_set$rating, unfactor(predicted))
err

hist(unfactor(predicted))

# save model
print("save the model")
save(multinom_bestfit, file="./mdl/multinom_bestfit.RData")

# restore warnings
options(warn = oldw)

print("job done")
