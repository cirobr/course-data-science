# R version: 4.1.0
setwd("~/projects/data-science-course/ds9-capstone")
subset_size = as.integer(nrow(train_set) * 0.1)
number_of_iterations = 300

# clean memory
rm(edx, edx2)
rm(vector, df, test_index, p, p1, p2)

# environment
library(ggplot2)
library(caret)
library(tidyverse)

options(digits = 3)

#rm(train_set, test_set)
# read dataset from csv
if(!exists("train_set")) {train_set <- read_csv(file = "./dat/train.csv")}
train_set$rating <- as.factor(train_set$rating)
if(!exists("test_set")) {test_set <- read_csv(file = "./dat/test.csv")}
test_set$rating <- as.factor(test_set$rating)

# creates train subset for experiences with small size dataset
train_subset <- head(train_set, n=subset_size)

# relevance of parameters
N = nrow(train_subset)
remove_columns <- colnames(train_subset)[1:3]
df <- train_subset %>% select(-remove_columns) %>% summarise(across(.cols = everything(), sum)) / N
df <- sort(df, decreasing = TRUE)
df <- data.frame(colnames(df), as.numeric(df))
colnames(df) <- c("Genres", "Proportion")

df %>% ggplot(aes(Genres, Proportion)) + 
  geom_bar(stat = "identity") +
  ggtitle("Relevance of Film Genres") +
  geom_text(aes(Genres, Proportion, label = Proportion), size=2.5, angle = 90, hjust = -0.1) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  #scale_y_continuous(labels = scales::percent)

# fit multi-class logistic regression
library(nnet)
log_fit <- multinom(rating ~ ., data = train_subset, maxit=number_of_iterations)
yh <- fitted(log_fit, test_set)
yh <- ifelse(yh <= 0.5, FALSE, TRUE)

res <- colnames(yh)
y_hat_log <- seq(0, 0, length.out = nrow(yh))
y_hat_log[which(yh[,res[1]])] <- as.numeric(res[1])
y_hat_log[which(yh[,res[2]])] <- as.numeric(res[2])
y_hat_log[which(yh[,res[3]])] <- as.numeric(res[3])
y_hat_log[which(yh[,res[4]])] <- as.numeric(res[4])
y_hat_log[which(yh[,res[5]])] <- as.numeric(res[5])
y_hat_log[which(yh[,res[6]])] <- as.numeric(res[6])
y_hat_log[which(yh[,res[7]])] <- as.numeric(res[7])
y_hat_log[which(yh[,res[8]])] <- as.numeric(res[8])
y_hat_log[which(yh[,res[9]])] <- as.numeric(res[9])
y_hat_log[which(yh[,res[10]])] <- as.numeric(res[10])

sum(class(test_set$rating) == as.factor(y_hat_log))
