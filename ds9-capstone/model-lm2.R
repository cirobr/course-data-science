# R version: 4.1.0
print("job start")

# suppress warnings
oldw <- getOption("warn")
options(warn = -1)

# clean memory
print("clean memory")
rm(edx, edx2)
rm(vector, df, test_index, p, p1, p2)

# environment
print("setup environment")
library(ggplot2)
library(plyr)
library(tidyverse)
library(caret)

options(digits = 3)
subset_size = 5000

# read csv datasets
print("read csv datasets")
if(!exists("train_set")) {train_set <- read_csv(file = "./dat/train.csv")}
if(!exists("test_set"))  {test_set  <- read_csv(file = "./dat/test.csv")}

# prepare datasets
#print("prepare datasets")
#df_train <- train_set %>% select(c(userId, movieId, rating))
#df_test  <- test_set %>% select(c(userId, movieId, rating))

# creates small size subset for experiences
# df <- head(train_set, n=subset_size)

# define error function
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

# define rating by class function
rbc <- function(ratings, group_name){
  db <- ratings %>% as.character() %>% data.frame()
  colnames(db) <- "ratings"
  db <- db %>% group_by(ratings) %>% summarize(qty = n())
  db$group <- group_name
  db <- db[,c("group", "ratings", "qty")]
  db
}

# store actual distribution by class on database
db <- rbc(test_set$rating, "1-test_set")

# fit the model
print("fit naive average model")
mu <- mean(train_set$rating)

print("predict outcome")
predicted <- round_any(rep(mu, nrow(test_set)), 0.5)

print("calculate error")
err <- RMSE(test_set$rating, predicted)
err

rmse_results <- data.frame(method = "2-naive_average",
                           RMSE = err)

db <- bind_rows(db, rbc(predicted, "2-naive_average"))

# fit the model
print("fit linear regression model")
lm_fit <- lm(rating ~ userId + movieId,
             data = train_set)

print("predict outcome")
predicted <- round_any(predict(lm_fit, test_set), 0.5)

print("calculate error")
err <- RMSE(test_set$rating, predicted)
err

rmse_results <- bind_rows(rmse_results, data.frame(method = "3-linear_regression",
                                                   RMSE = err))

db <- bind_rows(db, rbc(predicted, "3-linear_regression"))





# plot the chart
db %>%
  ggplot(aes(ratings, qty, fill=group)) +
  geom_bar(stat="identity", position = "dodge") +
  ggtitle("Proportion actual x predicted")

# restore warnings
options(warn = oldw)

print("job done")
