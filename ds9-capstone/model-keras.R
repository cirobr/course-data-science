# R version: 4.1.0
print("job start")

# suppress warnings
# oldw <- getOption("warn")
# options(warn = -1)

# environment
print("setup environment")
library(ggplot2)
library(dplyr)
library(tidyverse)

# keras environment
library(reticulate)
library(keras)
# Sys.setenv("RETICULATE_PYTHON" = "~/anaconda3/envs/r-env/bin/python")
use_condaenv("r-env")

options(digits = 3)

# define error function
errRMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

# read csv datasets
print("read csv datasets")
if(!exists("train_set")) {train_set <- read_csv(file = "./dat/train.csv") %>% as_tibble()}
if(!exists("test_set"))  {test_set  <- read_csv(file = "./dat/test.csv")  %>% as_tibble()}

# prepare datasets
print("prepare datasets")

# center and scale data
df_train <- train_set %>% mutate(across(2:4, scale)) %>% as.matrix() %>% as_tibble()
df_test  <- test_set  %>% mutate(across(2:4, scale)) %>% as.matrix() %>% as_tibble()

# predictors
X_train <- df_train %>% select(-c("rating")) %>% as.matrix()
X_test  <- df_test  %>% select(-c("rating")) %>% as.matrix()

# outcome (keras) factors [0 to 9]
y_train <- train_set$rating %>% as.factor() %>% as.numeric() - 1
y_test  <- test_set$rating  %>% as.factor() %>% as.numeric() - 1

# outcome levels
yLevels <- train_set$rating %>% as.factor() %>% levels()

# clean memory
rm(df_train, df_test)

# fit the model

# setup layers
D <- ncol(X_train)
model <- keras_model_sequential()

model %>%
  layer_dense(units = D, input_shape = c(D), activation = 'relu') %>%
  # layer_dropout(0.2) %>%
  layer_dense(units = 10, activation = 'softmax')

# compile the model
model %>% compile(
  optimizer = 'adam', 
  loss = 'sparse_categorical_crossentropy',
  metrics = c('accuracy')
)

# train the model
history <- model %>% fit(x = X_train, 
                         y = y_train, 
                         epochs = 30, 
                         validation_split = 0.2,
                         verbose = 2)

# load pre-built model to save execution time
# load(file="./mdl/keras_fit.RData")

# evaluate accuracy
plot(history)

score <- model %>% evaluate(X_test, y_test, verbose = 0)
score

# predict the outcome
print("predict the outcome")
predictedMatrix <- model %>% predict(X_test)
colnames(predictedMatrix) <- yLevels
predicted <- apply(predictedMatrix, 1, which.max)
predicted <- as.numeric(yLevels[predicted])

# calculate error metrics
print("calculate error metrics")
err <- errRMSE(test_set$rating, predicted)
err
hist(predicted)

# save model
# print("save model")
# save(keras_fit, file="./mdl/keras_fit.RData")

# cleanup
rm(X_train, X_test, y_train, y_test)

# restore warnings
options(warn = oldw)
