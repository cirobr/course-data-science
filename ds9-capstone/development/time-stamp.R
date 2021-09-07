library(lubridate)
library(tidyverse)

daysBetweenTimestamps <- function(x,y){
  difftime(as_date(as_datetime(x)), 
           as_date(as_datetime(y)), 
           units = c("days")) %>% 
    as.numeric()
}

# read dataset from csv
edx2 <- read_csv(file = "./dat/edx2.csv") %>% as_tibble()
head(edx2)

# first user rating
df <- edx2 %>% group_by(userId) %>%
  select(userId, timestamp) %>%
  summarize(firstUserRating = min(timestamp))

edx2 <- left_join(edx2, df)
head(edx2)

# first movie rating
df <- edx2 %>% group_by(movieId) %>%
  select(movieId, timestamp) %>%
  summarize(firstMovieRating = min(timestamp))

edx2 <- left_join(edx2, df)
head(edx2)

edx2 <- edx2 %>% mutate(daysFromFirstUserRating  = daysBetweenTimestamps(timestamp, firstUserRating),
                daysFromFirstMovieRating = daysBetweenTimestamps(timestamp, firstMovieRating)) %>%
  select(rating, timestamp, firstUserRating, daysFromFirstUserRating, firstMovieRating, daysFromFirstMovieRating)
