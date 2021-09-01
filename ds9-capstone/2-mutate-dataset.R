# R version: 4.1.0

# suppress warnings
# oldw <- getOption("warn")
# options(warn = -1)

# environment
print("setup environment")

library(ggplot2)
# library(lubridate)   # part of tidyverse
library(tidyverse)
library(caret)

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
         yearsToRate    = yearTimestamp - yearOfRelease) %>%
  select(-c(title))

# ratings per movie
df <- edx2 %>%
  group_by(movieId) %>%
  summarize(nrRatingsPerMovie = n(),
            avgRatingPerMovie = mean(rating))
edx2 <- left_join(edx2, df)

# ratings per user
df <- edx2 %>%
  group_by(userId) %>%
  summarize(nrRatingsPerUser = n(),
            avgRatingPerUser = mean(rating))
edx2 <- left_join(edx2, df)

# ratings per year of release
df <- edx2 %>%
  group_by(yearOfRelease) %>%
  summarize(nrRatingsPerYearOfRelease = n(),
            avgRatingPerYearOfRelease = mean(rating))
edx2 <- left_join(edx2, df)

# days from first rating (in progress)
# df <- edx2 %>%
#   group_by(movieId) %>%
#   summarize(timeOfFirstRating = min(timestamp),
#             daysFromFirstRating = day(as_datetime(timestamp) - astimeOfFirstRating))





# stop()

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
# rm(edx, df)
head(edx2)

