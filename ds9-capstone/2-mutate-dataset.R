# R version: 4.1.0

# suppress warnings
# oldw <- getOption("warn")
# options(warn = -1)

# environment
print("setup environment")

library(ggplot2)
library(lubridate)
library(tidyverse)
library(caret)
library(foreach)

options(digits = 3)

# clean memory
print("clean memory")
if(exists("validation")) {rm(validation)}

# read dataset from csv
if(!exists("edx")) {edx <- read_csv(file = "./dat/edx.csv") %>% as_tibble()}
head(edx)

# extract dates and add timeToRate
edx2 <- edx %>% 
  select(-c(genres)) %>%
  mutate(yearOfRelease = as.numeric(stringi::stri_sub(edx$title[1], -5, -2)),
         yearTimestamp = year(as_datetime(timestamp)),
         timeToRate    = yearTimestamp - yearOfRelease) %>%
  select(-c(title))

# ratings per movie
df <- edx2 %>%
  group_by(movieId) %>%
  summarize(ratingsPerMovie = n())
edx2 <- bind_cols(edx2, df)

stop()

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

