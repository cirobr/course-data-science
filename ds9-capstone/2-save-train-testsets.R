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

edx2 <- subset(edx, select = -c(timestamp, title, genres))
edx2$rating <- as.factor(edx2$rating)
edx2 <- cbind(edx2, df)
rm(vector, df, edx)
head(edx2)

# split edx in train and test sets
proportion = 0.20
set.seed(1, sample.kind = "Rounding")
test_index <- createDataPartition(edx2$rating, 
                                  times = 1, 
                                  p = proportion, 
                                  list = FALSE)
test_set <- edx2 %>% slice(test_index)
train_set <- edx2 %>% slice(-test_index)
rm(test_index, edx2)

train_set %>% as.data.frame() %>% write_csv(file = "./dat/train.csv")
test_set %>% as.data.frame() %>% write_csv(file = "./dat/test.csv")
