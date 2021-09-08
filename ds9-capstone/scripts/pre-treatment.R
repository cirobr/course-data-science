# R version: 4.1.0

# suppress warnings
oldw <- getOption("warn")
options(warn = -1)

# clean memory
print("clean memory")
rm(validation)

# environment
print("setup environment")

library(ggplot2)
library(lubridate)
library(tidyverse)
library(caret)
library(foreach)

options(digits = 3)

# cleanup memory
if(exists("validation")) {rm(validation)}

# read csv datasets
print("read csv datasets")
if(!exists("train_set")) {train_set <- read_csv(file = "./dat/train.csv") %>% as_tibble()}
if(!exists("test_set"))  {test_set  <- read_csv(file = "./dat/test.csv")  %>% as_tibble()}

# prepare datasets
print("pre-treatment")

# no NAs on trainset / testset
# any(is.na(df_train))
# any(is.na(df_test))

# remove movies and users from testset that are not present on trainset
test_set <- test_set %>%
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId")

# remove predictors with small variance
df_train <- train_set %>% select(-rating)
nzv <- nearZeroVar(df_train, foreach = TRUE, allowParallel = TRUE)
colnames(df_train[,nzv])
df_train <- df_train[,-nzv]
train_set <- bind_cols(rating=train_set$rating, df_train)
rm(df_train)

df_test <- test_set %>% select(-rating)
colnames(df_test[,nzv])
df_test <- df_test[,-nzv]
test_set <- bind_cols(rating=test_set$rating, df_test)
rm(df_test)

# save datasets
train_set %>% write_csv(file = "./dat/train.csv")
test_set  %>% write_csv(file = "./dat/test.csv")
head(train_set)
head(test_set)

# cleanup memory
rm(genres_names, proportion_test_set, vector, fn)

# restore warnings
options(warn = oldw)
