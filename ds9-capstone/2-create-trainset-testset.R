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
proportion_test_set = 0.20

# cleanup memory
if(exists("validation")) {rm(validation)}

# read dataset from csv
if(!exists("edx")) {edx <- read_csv(file = "./dat/edx.csv")}
edx <- as.tibble(edx)
head(edx)

# extract yearOfRelease, yearTimestamp, timeToRate as predictors
edx2 <- edx %>% 
  mutate(yearOfRelease = as.numeric(stringi::stri_sub(edx$title[1], -5, -2)),
         yearTimestamp = year(as_datetime(timestamp)),
         timeToRate    = yearTimestamp - yearOfRelease, .after = yearTimestamp) %>%
  select(-c(timestamp, title))

# extract movie genres as predictors
genres_names <- strsplit(edx2$genres, "|", fixed = TRUE) %>%
  unlist() %>%
  unique()

fn <- function(element_vector){
  as.numeric(grepl(element_vector, vector))
}

# remove hiphen from predictor names
vector <- edx2$genres
df <- sapply(genres_names, fn) %>% as.tibble()
colnames(df)[7]  <- "SciFi"
colnames(df)[16] <- "FilmNoir"
colnames(df)[20] <- "NoGenre"

edx2 <- edx2 %>% select(-genres) %>% bind_cols(df)
rm(edx, df)
head(edx2)

# split train and test sets
set.seed(1, sample.kind = "Rounding")
test_index <- createDataPartition(edx2$rating, 
                                  times = 1, 
                                  p = proportion_test_set,
                                  list = FALSE)
test_set <- edx2 %>% slice(test_index)
train_set <- edx2 %>% slice(-test_index)
rm(edx2, test_index)

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
rm(p, p1, p2)

# save split datasets
train_set %>% write_csv(file = "./dat/train.csv")
test_set  %>% write_csv(file = "./dat/test.csv")

# cleanup memory
rm(genres_names, proportion_test_set, vector, fn)

# restore warnings
options(warn = oldw)
