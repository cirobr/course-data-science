# R version: 4.1.0
print("job start")

# environment
print("setup environment")

# libraries
library(ggplot2)
library(lubridate)
library(tidyverse)
library(caret)

# keras environment
library(reticulate)
library(keras)
library(tfdatasets)
use_condaenv("r-gpu")

# global variables
options(digits = 3)

# tuning environment
FLAGS <- flags(
  flag_string("activationType", "relu"),
  flag_string("optimizerType", "adam")
)

# read csv datasets
print("read csv datasets")
if(!exists("train_set")) {train_set <- read_csv(file = "./dat/train.csv") %>% as_tibble()}
if(!exists("test_set"))  {test_set  <- read_csv(file = "./dat/test.csv")  %>% as_tibble()}

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
            biasUser))

# clean memory
rm(train_set, test_set)

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
# The patience parameter is the amount of epochs to check for improvement.
early_stop <- callback_early_stopping(monitor = "val_loss", patience = 20)

model <- build_model()

history <- model %>% fit(
  x = df_train %>% select(-deltaRating),
  y = df_train$deltaRating,
  epochs = 500,
  validation_split = 0.2,
  verbose = 0,
  callbacks = list(early_stop)
  )

plot(history)

# test the model
c(loss, mae) %<-% (model %>% 
                     evaluate(df_test %>% select(-label), test_df$label, verbose = 0))

