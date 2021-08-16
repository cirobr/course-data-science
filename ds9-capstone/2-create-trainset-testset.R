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
proportion_test_set = 0.30

# read dataset from csv
if(!exists("edx")) {edx <- read_csv(file = "./dat/edx.csv")}
head(edx)

# extract movie genres
genres_names <- strsplit(edx$genres, "|", fixed = TRUE) %>%
  unlist() %>%
  unique()

fn <- function(element_vector){
  as.numeric(grepl(element_vector, vector))
}

vector <- edx$genres
df <- sapply(genres_names,fn)
df <- as.data.frame(df)
colnames(df)[7]  <- "SciFi"
colnames(df)[16] <- "FilmNoir"
colnames(df)[20] <- "NoGenre"

edx2 <- subset(edx, select = -c(timestamp, title, genres))
edx2$rating <- as.factor(edx2$rating)
edx2 <- cbind(edx2, df)
head(edx2)

# split edx in train and test sets
set.seed(1, sample.kind = "Rounding")
test_index <- createDataPartition(edx2$rating, 
                                  times = 1, 
                                  p = proportion_test_set,
                                  list = FALSE)
test_set <- edx2 %>% slice(test_index)
train_set <- edx2 %>% slice(-test_index)

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

# save split datasets
train_set %>% as.data.frame() %>% write_csv(file = "./dat/train.csv")
test_set %>% as.data.frame() %>% write_csv(file = "./dat/test.csv")

# restore warnings
options(warn = oldw)
