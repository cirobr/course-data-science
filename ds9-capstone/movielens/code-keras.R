# R version: 4.1.0
print("job start")

# suppress warnings
oldw <- getOption("warn")
options(warn = -1)

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

# read csv datasets
print("read csv datasets")
if(!exists("train_set")) {train_set <- read_csv(file = "./dat/train.csv") %>% as_tibble()}
if(!exists("test_set"))  {test_set  <- read_csv(file = "./dat/test.csv")  %>% as_tibble()}

# define error function
errRMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

# below code crashes R
# lm_fit <- lm(rating ~ as.factor(userId) + as.factor(movieId),
#              data = train_set)

# predict by global average
mu <- mean(train_set$rating)
predicted <- mu
err <- RMSE(test_set$rating, predicted)
rmse_results <- data.frame(model = "naiveAvg",
                           error = err)

# add movie bias effect
df <- train_set %>%
  group_by(movieId) %>%
  summarize(deltaRating = mean(rating - mu))

qplot(deltaRating, data = df, bins = 10, color = I("black"))

dfBiasMovie <- left_join(train_set, df) %>%
  select(rating, movieId, deltaRating) %>%
  group_by(movieId) %>%
  summarize(biasMovie = mean(deltaRating))

df <- left_join(test_set, dfBiasMovie) %>%
  select(rating, movieId, biasMovie)
predicted = mu + df$biasMovie

err <- RMSE(test_set$rating, predicted)
rmse_results <- bind_rows(rmse_results,
                          data_frame(model ="movieBias",
                                     error = err))

# add user bias effect
df <- train_set %>%
  left_join(dfBiasMovie) %>%
  group_by(userId) %>%
  summarize(deltaRating = mean(rating - mu - biasMovie))

qplot(deltaRating, data = df, bins = 10, color = I("black"))

dfBiasUser <- left_join(train_set, df) %>%
  select(rating, userId, movieId, deltaRating) %>%
  group_by(userId) %>%
  summarize(biasUser = mean(deltaRating))

df <- left_join(test_set, dfBiasMovie) %>%
  left_join(dfBiasUser) %>%
  select(rating, movieId, userId, biasMovie, biasUser)
predicted = mu + df$biasMovie + df$biasUser

err <- RMSE(test_set$rating, predicted)
rmse_results <- bind_rows(rmse_results,
                          data_frame(model ="userBias",
                                     error = err))

# show RMSE results
rmse_results

# cleanup memory
rm(df, predicted)





# read csv datasets
print("read csv datasets")
if(!exists("train_set")) {train_set <- read_csv(file = "./dat/train.csv") %>% as_tibble()}
if(!exists("test_set"))  {test_set  <- read_csv(file = "./dat/test.csv")  %>% as_tibble()}

# prepare trainset
print("prepare trainset")
df_train <- train_set %>%
  left_join(dfBiasMovie) %>%
  left_join(dfBiasUser) %>%
  mutate(deltaRating = (rating - mu - biasMovie - biasUser), 
         .before = rating) %>%
  select(-c(rating, 
            userId, 
            movieId, 
            biasMovie, 
            biasUser))

# remove predictors with small variance
# print("remove small variance predictors")
# nzv <- df_train %>% 
#   select(-deltaRating) %>%
#   nearZeroVar(foreach = TRUE, allowParallel = TRUE)
# 
# removedPredictors <- colnames(df_train[,nzv])
# df_train <- df_train %>% select(-all_of(removedPredictors))

# prepare testset
print("prepare testset")
df_test <- test_set %>%
  left_join(dfBiasMovie) %>%
  left_join(dfBiasUser) %>%
  mutate(deltaRating = (rating - mu - biasMovie - biasUser), 
         .before = rating) %>%
  select(-c(rating, 
            userId, 
            movieId, 
            biasMovie, 
            biasUser))
  #select(-all_of(removedPredictors))

# special
df_train <- df_train %>%
  select(deltaRating:daysFromFirstMovieRating)
df_test <- df_test %>%
  select(deltaRating:daysFromFirstMovieRating)

# clean memory
rm(train_set)

# scale predictors
print("scale predictors")
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
    layer_dense(units = 16, activation = "relu") %>%
    layer_dense(units = 16, activation = "relu") %>%
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
print("fit the model")

print_dot_callback <- callback_lambda(
  on_epoch_end = function(epoch, logs) {
    if (epoch %% 80 == 0) cat("\n")
    cat(".")
  }
)    

early_stop <- callback_early_stopping(monitor = "val_loss",
                                      min_delta = 1e-5,
                                      patience = 5)

model <- build_model()

history <- model %>% fit(
  x = df_train %>% select(-deltaRating),
  y = df_train$deltaRating,
  epochs = 50,
  validation_split = 0.2,
  verbose = 1,
  callbacks = list(early_stop) # print_dot_callback
  )

plot(history)

# save model
print("save model")
save_model_tf(model, filepath = "./mdl/keras-fit")

# load model
# load_model_tf(model, filepath = "./mdl/keras-fit")

# predict
print("predict testset results")
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

rmse_results <- bind_rows(rmse_results,
                          data_frame(model ="keras",
                                     error = err))

# show RMSE results
rmse_results

# restore warnings
options(warn = oldw)

print("job done")
