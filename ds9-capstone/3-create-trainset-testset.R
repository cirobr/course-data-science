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
library(tidyverse)
library(caret)

options(digits = 3)
proportionTestSet = 0.20

# read dataset from csv
if(!exists("edx2")) {edx2 <- read_csv(file = "./dat/edx2.csv") %>% as.tibble()}
head(edx2)

# split train and test sets
set.seed(1, sample.kind = "Rounding")
test_index <- createDataPartition(edx2$rating, 
                                  times = 1, 
                                  p = proportionTestSet,
                                  list = FALSE)
test_set <- edx2 %>% slice(test_index)
train_set <- edx2 %>% slice(-test_index)

# remove movies and users from testset that are not present on trainset
test_set <- test_set %>%
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId")

# check for stratification of train / test split
p1 <- train_set %>%
  group_by(rating) %>%
  summarize(qty = n()) %>%
  mutate(split = 'train_set')

p2 <- test_set %>%
  group_by(rating) %>%
  summarize(qty = n()) %>%
  mutate(split = 'test_set')

p <- bind_rows(p1, p2) %>% group_by(split)
p %>% ggplot(aes(rating, qty, fill = split)) +
  geom_bar(stat="identity", position = "dodge") +
  ggtitle("Stratification of Test_set / Train_set split")

# cleanup memory
rm(edx2, test_index)
rm(p, p1, p2)

# save datasets
train_set %>% write_csv(file = "./dat/train.csv")
test_set  %>% write_csv(file = "./dat/test.csv")

# restore warnings
options(warn = oldw)
