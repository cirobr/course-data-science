# R version: 4.1.0

# suppress warnings
oldw <- getOption("warn")
options(warn = -1)

# environment
print("setup environment")

# one-function libraries
# library(stringi)     # used as stringi::stri_sub()

# libraries
library(ggplot2)
library(lubridate)
library(tidyverse)
library(caret)

# global variables
options(digits = 3)

# clean memory
print("clean memory")
if(exists("validation")) {rm(validation)}

# read dataset from csv
if(!exists("edx")) {edx <- read_csv(file = "./dat/edx.csv") %>% as_tibble()}
head(edx)

# move ratings to first column
edx2 <- edx %>% select(-c(rating))
edx2 <- cbind(rating = edx$rating, edx2)

# extract yearOfRelease and timestampYear
edx2 <- edx2 %>% 
  select(-c(genres)) %>%
  mutate(yearOfRelease = as.numeric(stringi::stri_sub(edx$title[1], -5, -2)),
         timestampYear = year(as_datetime(timestamp)),
         yearsFromRelease = timestampYear - yearOfRelease) %>%
  select(-c(title, yearOfRelease, timestampYear)) 

# function definition
daysBetweenTimestamps <- function(x,y){
  difftime(as_date(as_datetime(x)), 
           as_date(as_datetime(y)), 
           units = c("days")) %>% 
    as.numeric()
}

# extract firstUserRating
df <- edx2 %>% group_by(userId) %>%
  select(userId, timestamp) %>%
  summarize(firstUserRating = min(timestamp))

edx2 <- left_join(edx2, df)

# extract firstMovieRating
df <- edx2 %>% group_by(movieId) %>%
  select(movieId, timestamp) %>%
  summarize(firstMovieRating = min(timestamp))

edx2 <- left_join(edx2, df)

# extract daysFromFirstUserRating and daysFromFirstMovieRating
edx2 <- edx2 %>% mutate(daysFromFirstUserRating  = daysBetweenTimestamps(timestamp, firstUserRating),
                        daysFromFirstMovieRating = daysBetweenTimestamps(timestamp, firstMovieRating)) %>%
  select(-c(timestamp, firstUserRating,firstMovieRating))

# extract movie genres as predictors
genres_names <- strsplit(edx$genres, "|", fixed = TRUE) %>%
  unlist() %>%
  unique()

fn <- function(element_vector){
  as.numeric(grepl(element_vector, vector))
}

vector <- edx$genres
df <- sapply(genres_names, fn) %>% as_tibble()

# remove hiphen from predictor names
colnames(df)[7]  <- "SciFi"
colnames(df)[16] <- "FilmNoir"
colnames(df)[20] <- "NoGenre"

edx2 <- bind_cols(edx2, df)
head(edx2)

# save datasets
edx2 %>% write_csv(file = "./dat/edx2.csv")

# cleanup memory
rm(df, edx, genres_names, vector, fn, daysBetweenTimestamps)

# restore warnings
options(warn = oldw)
