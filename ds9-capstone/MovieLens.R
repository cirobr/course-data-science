# R version: 4.1.0
setwd("~/projects/data-science-course/ds9-capstone")

# libraries
library(ggplot2)
library(caret)
library(tidyverse)

# read edx dataset from csv
if(!exists("edx")) {edx <- read_csv(file = "./dat/edx.csv")}
head(edx)

# extract movie genres
genres_names <- strsplit(edx$genres, "|", fixed = TRUE) %>%
  unlist() %>%
  unique()

fn <- function(element_vector){
  grepl(element_vector, vector)
}

vector <- edx$genres
df <- sapply(genres_names,fn)

df <- as.data.frame(df)
edx2 <- subset(edx, select = -c(genres))
edx2 <- cbind(edx2, df)
rm(df, edx)
head(edx2)

# split edx in train and test sets
y = edx2$rating              # outcome to predict: movie rating for a given user
proportion = 0.20
set.seed(1, sample.kind = "Rounding")
test_index <- createDataPartition(y, times = 1, p = proportion, list = FALSE)
test_set <- edx2 %>% slice(test_index)
train_set <- edx2 %>% slice(-test_index)

# check for stratification of train / test splits
p1 <- train_set %>%
  group_by(rating) %>%
  summarize(qty = n()) %>%
  mutate(split = 'train_set')

p2 <- test_set %>%
  group_by(rating) %>%
  summarize(qty = n()) %>%
  mutate(split = 'test_set')

p <- bind_rows(p1, p2) %>% group_by(split)
p$rating <- as.factor(p$rating)
p %>% ggplot(aes(rating, qty, fill = split)) +
  geom_bar(stat="identity", position = "dodge") +
  ggtitle("Stratification of Train_set / Test_set split")

