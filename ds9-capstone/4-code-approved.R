# R version: 4.1.0
print("job start")

# suppress warnings
oldw <- getOption("warn")
options(warn = -1)

# environment
print("setup environment")

# one-function libraries
# library(plyr)                 # used as plyr::round_any()
# library(varhandle)            # used as varhandle::unfactor()

# libraries
library(ggplot2)
library(lubridate)
library(tidyverse)
library(caret)

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

# restore warnings
options(warn = oldw)

print("job done")
