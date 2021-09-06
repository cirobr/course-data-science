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

# keras environment
library(reticulate)                         # interface R and Python
use_condaenv("r-gpu", required = TRUE)      # conda env for running tf and keras
library(keras)
library(tfdatasets)

# global variables
options(digits = 5)

# tuning environment
FLAGS <- flags(
  flag_string("activationType", "relu"),
  flag_string("optimizerType", "adam")
)

# read csv datasets
print("read csv datasets")
if(!exists("train_set")) {train_set <- read_csv(file = "./dat/train.csv") %>% as_tibble()}
if(!exists("test_set"))  {test_set  <- read_csv(file = "./dat/test.csv")  %>% as_tibble()}

# prepare trainset
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

# remove predictors with small variance
nzv <- df_train %>% 
  select(-deltaRating) %>%
  nearZeroVar(foreach = TRUE, allowParallel = TRUE)

removedPredictors <- colnames(df_train[,nzv])
df_train <- df_train %>% select(-all_of(removedPredictors))

# prepare testset
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
  select(-all_of(removedPredictors))

# clean memory
rm(train_set)

# scale predictors
spec <- feature_spec(df_train, deltaRating ~ . ) %>% 
  step_numeric_column(all_numeric(), normalizer_fn = scaler_standard()) %>% 
  fit()
spec

# wrap the model in a function
build_model <- function() {
  # create model
  input <- layer_input_from_dataset(df_train %>% select(-deltaRating))
  
  output <- input %>% 
    layer_dense_features(dense_features(spec)) %>% 
    layer_dense(units = 64, activation = "relu") %>%
    layer_dense(units = 64, activation = "relu") %>%
    layer_dense(units = 1) 
  
  model <- keras_model(input, output)
  summary(model)
  
  # compile model
  model %>% 
    compile(
      loss = "mse",
      optimizer = optimizer_rmsprop(),
      metrics = list("mean_absolute_error")
    )
  
  model
}

# train the model
# early_stop <- callback_early_stopping(monitor = "val_loss", 
#                                       min_delta = 0.0001,
#                                       patience = 5)
# 
# model <- build_model()
# 
# history <- model %>% fit(
#   x = df_train %>% select(-deltaRating),
#   y = df_train$deltaRating,
#   epochs = 50,
#   validation_split = 0.2,
#   verbose = 1,
#   callbacks = list(early_stop)   # print_dot_callback
#   )
# 
# plot(history)
# 
# # save model
# print("save model")
# save_model_tf(model, filepath = "./mdl/keras_fit")

# load model
load_model_tf(model, filepath = "./mdl/keras_fit")

# test the model
c(loss, mae) %<-% (model %>% 
                     evaluate(df_test %>% select(-deltaRating), 
                              df_test$deltaRating, 
                              verbose = 0))

# predict
p <- model %>% predict(df_test %>% select(-deltaRating))
p <- p[ , 1]

df <- test_set %>%
  select(rating, userId, movieId) %>%
  left_join(dfBiasMovie) %>%
  left_join(dfBiasUser) %>%
  mutate(predicted = mu + biasMovie + biasUser + p)

# calculate error metrics
print("calculate error metrics")
err <- errRMSE(test_set$rating, df$predicted)
err

