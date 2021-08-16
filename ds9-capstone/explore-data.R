# R version: 4.1.0
print("job start")

# environment
print("setup environment")
library(ggplot2)
library(plyr)
library(tidyverse)
library(caret)
library(nnet)
library(randomForest)

options(digits = 3)
subset_size = 5000

# environment
print("setup environment")
number_of_iterations <- 250

# read csv datasets
print("read csv datasets")
if(!exists("train_set")) {train_set <- read_csv(file = "./dat/train.csv")}
# if(!exists("test_set"))  {test_set  <- read_csv(file = "./dat/test.csv")}

# prepare datasets
print("prepare datasets")
df_train <- train_set # %>% select(c(userId, movieId, rating))
# df_test  <- test_set  # %>% select(c(userId, movieId, rating))

df_train$rating <- as.factor(df_train$rating)
# df_test$rating  <- as.factor(df_test$rating)

# creates small subset for experiments
df <- head(df_train, n=subset_size)

# define rating by class function
rbc <- function(ratings, group_name){
  db <- ratings %>% as.character() %>% data.frame()
  colnames(db) <- "ratings"
  db <- db %>% group_by(ratings) %>% summarize(qty = n())
  db$group <- group_name
  db <- db[,c("group", "ratings", "qty")]
  db
}

# define function for plotting distribution by rating
plt <- function(db){
  db %>%
    ggplot(aes(ratings, qty, fill=group)) +
    geom_bar(stat="identity", position = "dodge") +
    ggtitle("Proportion actual x predicted")
}

plt(rbc(df_train$rating, "train_set"))

# find logical predictors with frequency higher than threshold
threshold <- 0.01
nr <- nrow(train_set)
head(train_set)

threshold_genres <- train_set %>% 
  select(-c(userId, movieId, rating)) %>%
  colSums() / nr %>%
  sort(decreasing = TRUE)
threshold_genres <- tibble(genres=names(threshold_columns), p=threshold_columns) %>%
  filter(p > threshold) %>% 
  select(genres)
threshold_genres <- threshold_genres$genres
threshold_genres

relevant_columns <- c(c("userId", "movieId", "rating"), threshold_genres)
relevant_columns
