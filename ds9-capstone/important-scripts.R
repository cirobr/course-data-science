# # sorted genres of movies
# sortedGenres <- df_train %>% 
#   select(-c(rating, userId, movieId, year_stamp)) %>% 
#   as.matrix %>% 
#   colSums() %>% 
#   sort(decreasing = TRUE)
# 
# estimation of biases
dfUser <- df_train %>%
  group_by(userId) %>%
  summarize(meanUserRating = mean(rating))

dfMovie <- df_train %>%
  group_by(movieId) %>%
  summarize(meanMovieRating = mean(rating))

dfDramaUser <- df_train %>%
  select(c(rating, userId, movieId, Drama)) %>%
  filter(Drama == 1) %>%
  group_by(userId) %>%
  summarize(meanDramaUserRating = mean(rating))

dfDramaMovie <- df_train %>%
  select(c(rating, userId, movieId, Drama)) %>%
  filter(Drama == 1) %>%
  group_by(movieId) %>%
  summarize(meanDramaMovieRating = mean(rating))




df_train <- left_join(df_train, dfUser)
df_train <- left_join(df_train, dfMovie)
# 
# df_train <- left_join(df_train, dfDramaUser)
# df_train <- left_join(df_train, dfDramaMovie)
# 
# df_train[is.na(df_train)] <- -1

# stop()

