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
library(caret)
library(tidyverse)
library(nnet)

options(digits = 3)
subset_size = 50000
number_of_iterations <- 500

# read csv datasets
print("read csv datasets")
train_set <- read_csv(file = "./dat/train.csv")
test_set <- read_csv(file = "./dat/test.csv")

# pca analysis
print("execute pca")
df <- train_set %>% select(-c(rating))
res.pca <- prcomp(df, center = TRUE, scale. = TRUE)
s <- summary(res.pca)
ind <- max(which(s$importance[3,] < 0.95))

# prepare datasets
print("prepare datasets")
train_set_pca <- s$x[,1:ind] %>% as.data.frame()
train_set_pca <- cbind(rating = train_set$rating, train_set_pca)
train_set_pca$rating <- as.factor(train_set_pca$rating)

df <- test_set %>% select(-c(rating))
test_set_pca <- predict(res.pca, df)[,1:ind] %>% as.data.frame()
test_set_pca <- cbind(rating = test_set$rating, test_set_pca)
test_set_pca$rating <- as.factor(test_set_pca$rating)

rm(df)

# creates small subset for experiments
# df <- head(train_set, n=subset_size)

# fit the model
print("fit the model")
# multinom_pca_fit <- train_set_pca %>%
#   multinom(rating ~ .,
#            data = .,
#            maxit=number_of_iterations)
load(file="./mdl/multinom_pca_fit.RData")

# predict the outcome
print("predict the outcome")
y_hat <- predict(multinom_pca_fit, test_set_pca)

# rmse function definition
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

# calculate error metrics
print("calculate error metrics")
RMSE(as.numeric(test_set_pca$rating), as.numeric(y_hat))

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
# print("save the model")
# save(multinom_pca_fit, file="./mdl/multinom_pca_fit.RData")

# restore warnings
options(warn = oldw)

print("job done")
