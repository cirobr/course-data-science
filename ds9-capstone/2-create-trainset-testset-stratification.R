# write datasets to csv
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
colnames(df)[20] <- "No-Genre"

edx2 <- subset(edx, select = -c(timestamp, title, genres))
edx2$rating <- as.factor(edx2$rating)
edx2 <- cbind(edx2, df)
rm(vector, df, edx)
head(edx2)

# check relevance of predictors
nr <- nrow(edx2)
df <- edx2 %>% select(-c(userId, movieId, rating)) %>% colSums() /nr * 100
df <- tibble(Genres=names(df), Percentage=df)
df <- df %>% arrange(desc(Percentage))
df

# removing predictors < 4%
edx2 <- edx2 %>% select(-c("Western", "Film-Noir", "Documentary", "IMAX", "No-Genre"))
nr <- nrow(edx2)
df <- edx2 %>% select(-c(userId, movieId, rating)) %>% colSums() /nr * 100
df <- tibble(Genres=names(df), Percentage=df)
df <- df %>% arrange(desc(Percentage))
df

edx2 %>% as.data.frame() %>% write_csv(file = "./dat/edx2.csv")

# split edx in train and test sets
proportion = 0.25
set.seed(1, sample.kind = "Rounding")
test_index <- createDataPartition(edx2$rating, 
                                  times = 1, 
                                  p = proportion, 
                                  list = FALSE)
test_set <- edx2 %>% slice(test_index)
train_set <- edx2 %>% slice(-test_index)
rm(test_index)

train_set %>% as.data.frame() %>% write_csv(file = "./dat/train.csv")
test_set %>% as.data.frame() %>% write_csv(file = "./dat/test.csv")

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
rm(p1, p2, test_set, edx2)
