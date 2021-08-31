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
library(caret)

# keras environment
library(reticulate)
library(keras)

options(digits = 3)
subset_size = 5000

# define error function
errRMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

# read csv datasets
print("read csv datasets")
if(!exists("train_set")) {train_set <- read_csv(file = "./dat/train.csv") %>% as_tibble()}
# if(!exists("test_set"))  {test_set  <- read_csv(file = "./dat/test.csv") %>% as_tibble()}

# prepare datasets
print("prepare datasets")
df_train <- train_set
# df_test  <- test_set

# dataset for training
df_train <- df_train %>% select(rating:timeToRate)

# augment dataset
# estimation of biases
dfUser <- df_train %>%
  group_by(userId) %>%
  summarize(meanUserRating = mean(rating))

dfMovie <- df_train %>%
  group_by(movieId) %>%
  summarize(meanMovieRating = mean(rating))

df_train <- left_join(df_train, dfUser)
df_train <- left_join(df_train, dfMovie)



dfDramaUser <- train_set %>%
  select(c(rating, userId, movieId, Drama)) %>%
  filter(Drama == 1) %>%
  group_by(userId) %>%
  summarize(meanDramaUserRating = mean(rating))

dfDramaMovie <- train_set %>%
  select(c(rating, userId, movieId, Drama)) %>%
  filter(Drama == 1) %>%
  group_by(movieId) %>%
  summarize(meanDramaMovieRating = mean(rating))

df_train <- left_join(df_train, dfDramaUser)
df_train <- left_join(df_train, dfDramaMovie)

df_train[is.na(df_train)] <- -1

# scale predictors and factor outcomes
# outcomeFactors <- c(0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5)

# df_train$rating <- factor(df_train$rating, levels = outcomeFactors)
y_train <- train_set$rating
X_train <- df_train[-1] %>% as.matrix() %>% scale()

# cleanup memory
# rm(df_train)

# setup layers
D <- ncol(X_train)
model <- keras_model_sequential()

model %>%
  layer_dense(units = D, input_shape = c(D), activation = 'relu') %>%
  layer_dropout(0.2) %>%
  layer_dense(units = 64, activation = 'softmax')

# compile the model
model %>% compile(
  optimizer = 'adam', 
  loss = 'sparse_categorical_crossentropy',
  metrics = c('accuracy')
)

# train the model
history <- model %>% fit(x = X_train,
                         y = y_train, 
                         epochs = 5,              # estudar critério de parada
                         validation_split = 0.2,
                         verbose = 2)

# evaluate accuracy
plot(history)

