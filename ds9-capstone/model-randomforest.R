# R version: 4.1.0

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
library(caret)
library(tidyverse)
library(randomForest)

options(digits = 3)
subset_size = 50000

# read csv datasets
print("read csv datasets")

train_set <- read_csv(file = "./dat/train.csv") %>% as.tibble()
test_set  <- read_csv(file = "./dat/test.csv") %>% as.tibble()

# prepare datasets
print("prepare datasets")

rating <- as.factor(train_set$rating)
train_set <- train_set %>% select(-c(rating)) %>% mutate(across(userId:NoGenre, as.integer))
train_set <- bind_cols(rating=rating, train_set)
#head(train_set)

rating <- as.factor(test_set$rating)
test_set <- test_set %>% select(-c(rating)) %>% mutate(across(userId:NoGenre, as.integer))
test_set <- bind_cols(rating=rating, test_set)
#head(test_set)

# creates small subset for experiments
# df <- head(train_set, n=subset_size)

# fit the model
print("fit the model")

rf_fit <- randomForest(rating ~ .,
                       ntree = 50,
                       proximity = FALSE,
                       data = train_set)
                       #data = df)

# load(file="./mdl/rf_fit.RData")

# predict the outcome
print("predict the outcome")
y_hat <- predict(rf_fit, test_set, type = "class")

# rmse function definition
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

# calculate error metrics
print("calculate error metrics")
RMSE(as.numeric(test_set$rating), as.numeric(y_hat))

# confusion matrix analysis
cm <- confusionMatrix(y_hat, test_set$rating)
cm

# proportion of ratings at testset
db1 <- test_set$rating %>% as.character() %>% data.frame()
colnames(db1) <- "ratings"
db1 <- db1 %>% group_by(ratings) %>% summarize(qty = n())
db1$group <- "actual"
db1 <- db1[,c("ratings", "group", "qty")]

# proportion of predicted ratings
db2 <- y_hat %>% as.character() %>% data.frame()
colnames(db2) <- "ratings"
db2 <- db2 %>% group_by(ratings) %>% summarize(qty = n())
db2$group <- "predicted"
db2 <- db2[,c("ratings", "group", "qty")]
db <- bind_rows(db1, db2)

# plot the chart
db %>%
  ggplot(aes(ratings, qty, fill=group)) +
  geom_bar(stat="identity", position = "dodge") +
  ggtitle("Proportion actual x predicted")

# save model
print("save the model")
save(rf_fit, file="./mdl/rf_fit.RData")

# restore warnings
options(warn = oldw)
