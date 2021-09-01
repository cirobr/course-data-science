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


