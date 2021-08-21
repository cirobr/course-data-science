# R version: 4.1.0
print("job start")

# suppress warnings
# oldw <- getOption("warn")
# options(warn = -1)

# environment
print("setup environment")
# library(plyr)                 # used as plyr::round_any()
# library(varhandle)            # used as varhandle::unfactor()
library(ggplot2)
library(dplyr)
library(tidyverse)
#library(caret)
#library(randomForest)
#library(nnet)
#library(BBmisc)
library(keras)
# library(tensorflow)
library(doParallel)

options(digits = 3)
subset_size = 500000

# define error function
errRMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

# paralell processing
# number_of_processor_cores = 2
# cl <- makePSOCKcluster(number_of_processor_cores)

# read csv datasets
print("read csv datasets")
if(!exists("train_set")) {train_set <- read_csv(file = "./dat/train.csv") %>% as_tibble()}
if(!exists("test_set"))  {test_set  <- read_csv(file = "./dat/test.csv") %>% as_tibble()}

# prepare datasets
print("prepare datasets")

# predictors
df_train <- train_set %>% select(-c("rating", "year_stamp")) %>% as.matrix()
df_test  <- test_set  %>% select(-c("rating", "year_stamp")) %>% as.matrix()

# outcome labels
df_train_labels <- train_set$rating %>% as.numeric()
df_test_labels  <- test_set$rating  %>% as.numeric()

# center and scale data
# nc <- ncol(df_train)
# df_train <- df_train %>% mutate(across(1:nc, scale)) %>% as.matrix()
# df_test  <- df_test  %>% mutate(across(1:nc, scale)) %>% as.matrix()

# creates small subset for experiments
# df <- head(df_train, n=subset_size)
# df_labels <- head(df_train_labels, n=subset_size)

# fit the model

# setup layers
model <- keras_model_sequential()
model %>%
  layer_dense(units = 128, activation = 'relu') %>%
  # layer_dropout(0.2) %>%
  layer_dense(units = 10, activation = 'softmax')

# compile the model
model %>% compile(
  optimizer = 'adam', 
  loss = 'sparse_categorical_crossentropy',
  metrics = c('accuracy')
)

# train the model
model %>% fit(df_train, df_train_labels, epochs = 5, verbose = 2)

# evaluate accuracy
score <- model %>% evaluate(df_test, df_test_labels, verbose = 0)
score

# predict the outcome
print("predict the outcome")
# predictedMatrix <- model %>% predict(df_test)
predicted <- model %>% predict_classes(df_test)

# calculate error metrics
print("calculate error metrics")
err <- errRMSE(test_set$rating, predicted)
err
hist(predicted)

