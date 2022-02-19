# This R Script provides codes used for training different models
# to build a movie recommendation system. These codes are also included
# in the Rmarkdown and PDF documents that contain details and 
# explanations about the datasets, models and their results.

##################################
# Preliminaries                  #
# Create edx set, validation set #
##################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) 
  install.packages("tidyverse", 
                   repos = "http://cran.us.r-project.org")
if(!require(caret)) 
  install.packages("caret", 
                   repos = "http://cran.us.r-project.org")
if(!require(data.table)) 
  install.packages("data.table", 
                   repos = "http://cran.us.r-project.org")

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", 
                             readLines(unzip(dl, 
                                             "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), 
                          "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")
movies <- as.data.frame(movies, #For R 4.0, set stringsAsFactors as TRUE
                        stringsAsFactors = TRUE) %>% 
  mutate(movieId = as.numeric(levels(movieId))[movieId],
         title = as.character(title),
         genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding")
# If using R 3.5 or earlier, use `set.seed(1)` instead
test_index <- createDataPartition(y = movielens$rating, 
                                  times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)

###########################################
# Create train and test sets from edx set #
###########################################
# Test set will be 10% of edx data
set.seed(1, sample.kind="Rounding")
# If using R 3.5 or earlier, use `set.seed(1)` instead
test_index <- createDataPartition(y = edx$rating, times = 1, p = 0.1, 
                                  list = FALSE)
train_set <- edx[-test_index,]
temp <- edx[test_index,]

# Make sure userId and movieId in test set are also in train set
test_set <- temp %>% 
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId")

# Add rows removed from test set back into train set
removed <- anti_join(temp, test_set)
train_set <- rbind(train_set, removed)

rm(test_index, temp, removed)

# Data exploration in edx set
dim(edx)
head(edx) %>% knitr::kable()

# Number of users in edx set
n_distinct(edx$userId)

# Number of movies in edx set
n_distinct(edx$movieId)

# Rating distribution in edx set
edx %>% group_by(rating) %>%
  ggplot(aes(x = rating)) +
  geom_histogram() + 
  ggtitle("Distribution of Ratings") +
  xlab("Rating") +
  ylab("Number of Ratings")

# Rating distribution by year in edx set
if(!require(lubridate)) 
  install.packages("lubridate", repos = "http://cran.us.r-project.org")
edx %>% mutate(year = year(as_datetime(timestamp, 
                                       origin = "1970-01-01"))) %>%
  ggplot(aes(x = year)) +
  geom_histogram() + 
  ggtitle("Distribution of Ratings Per Year") +
  xlab("Year") +
  ylab("Number of Ratings")

# Main genres in edx set
genre = c("Action", "Adventure", "Animation", "Children", 
          "Comedy", "Crime", "Drama", "Fantasy", "Film-Noir", 
          "Horror", "IMAX", "Musical", "Mystery", "Romance",
          "Sci-Fi", "Thriller", "War", "Western")
sapply(genre, function(g) {
  sum(str_detect(edx$genres, g))
}) %>% knitr::kable()

# Movie that were rated most times by users in edx set
edx %>% group_by(title) %>% summarize(count = n()) %>% 
  arrange(desc(count)) %>% head(10) %>% knitr::kable()

# Create a function that computes the RMSE for vectors of ratings 
# and their corresponding predictors
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

#############################
# Training different models #
#############################

###############
# First model #
###############
# Assumes the same rating for all movies and users 
# with all the differences explained by random variation
# Average of all ratings
mu_hat <- mean(train_set$rating)
mu_hat

#Calculate a naive RMSE
naive_rmse <- RMSE(test_set$rating, mu_hat)
naive_rmse

################
# Second model #
################
# Model with movie effects
# Estimate average ranking for movie  i
mu <- mean(train_set$rating) 
movie_avgs <- train_set %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - mu))

# See movie effect estimates graphically
qplot(b_i, data = movie_avgs, bins = 30, color = I("black"), 
      main = "b_i estimates")

# Predict ratings with movie effects
predicted_ratings_m <- mu + test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  pull(b_i)

# Estimate RMSE
m_rmse <- RMSE(test_set$rating, predicted_ratings_m)
m_rmse

###############
# Third model # 
###############
# Model with movie and user effects
# Estimate average ranking for user u
user_avgs <- train_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))

# See user effect estimates graphically
qplot(b_u, data = user_avgs, bins = 30, color = I("black"), 
      main = "b_u estimates")

# Predict ratings with movie and user effects
predicted_ratings_m_u <- test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(pred = mu + b_i + b_u) %>%
  pull(pred)

# Estimate RMSE
m_u_rmse <- RMSE(test_set$rating, predicted_ratings_m_u)
m_u_rmse

################
# Fourth model # 
################
# Model with movie, user and genre effects
# Estimate genre-specific effect
mu <- mean(train_set$rating) 
genre_avgs <- train_set %>%
  left_join(movie_avgs, by="movieId") %>%
  left_join(user_avgs, by='userId') %>%
  group_by(genres) %>%
  summarize(b_g = mean(rating - mu - b_i - b_u))

# See genre effect estimates graphically
qplot(b_g, data = genre_avgs, bins = 30, color = I("black"), 
      main = "b_g estimates")  

# Predict ratings with genre effects
predicted_ratings_m_u_g <- test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  left_join(genre_avgs, by='genres') %>%
  mutate(pred = mu + b_i + b_u + b_g) %>%
  pull(pred)

# Estimate RMSE
m_u_g_rmse <- RMSE(test_set$rating, predicted_ratings_m_u_g)
m_u_g_rmse

###############
# Fifth model # 
###############
# Model with penalized least squares (regularization)
# Choosing the penalty term for movie effects using cross-validation
lambdas <- seq(0, 10, 0.25)

mu <- mean(train_set$rating)
just_the_sum <- train_set %>% 
  group_by(movieId) %>% 
  summarize(s = sum(rating - mu), n_i = n())

rmses_reg_m <- sapply(lambdas, function(l){
  predicted_ratings <- test_set %>% 
    left_join(just_the_sum, by='movieId') %>% 
    mutate(b_i = s/(n_i+l)) %>%
    mutate(pred = mu + b_i) %>%
    pull(pred)
  return(RMSE(test_set$rating, predicted_ratings))
})

qplot(lambdas, rmses_reg_m)  
lambdas[which.min(rmses_reg_m)]
min(rmses_reg_m)

###############
# Sixth model # 
###############
# Model with penalized least squares (regularization)
# Choosing the penalty term for movie and user effects using cross-validation
lambdas <- seq(0, 10, 0.25)

rmses_reg_m_u <- sapply(lambdas, function(l){
  mu <- mean(train_set$rating)
  b_i <- train_set %>% 
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  b_u <- train_set %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  predicted_ratings <- 
    test_set %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = mu + b_i + b_u) %>%
    pull(pred)
  return(RMSE(test_set$rating, predicted_ratings))
})

qplot(lambdas, rmses_reg_m_u)
lambdas[which.min(rmses_reg_m_u)]
min(rmses_reg_m_u)

#################
# Seventh model #
#################
# Model with penalized least squares (regularization)
# Choosing the penalty term for movie, user and genre effects 
# using cross-validation
lambdas <- seq(0, 10, 0.25)

rmses_reg_m_u_g <- sapply(lambdas, function(l){
  mu <- mean(train_set$rating)
  b_i <- train_set %>% 
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  b_u <- train_set %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - mu - b_i)/(n()+l))
  b_g <- train_set %>% 
    left_join(b_i, by="movieId") %>%
    left_join(b_u, by="userId") %>%
    group_by(genres) %>%
    summarize(b_g = sum(rating - mu - b_i - b_u)/(n()+l))
  predicted_ratings <- 
    test_set %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    left_join(b_g, by = "genres") %>%
    mutate(pred = mu + b_i + b_u + b_g) %>%
    pull(pred)
  return(RMSE(test_set$rating, predicted_ratings))
})

qplot(lambdas, rmses_reg_m_u_g)
lambdas[which.min(rmses_reg_m_u_g)]
min(rmses_reg_m_u_g)

# Comparing models
rmse_results <- tibble(method = c("Just the average", "Movie Effects Model",
                                  "Movie + User Effects Model",
                                  "Movie + User + Genre Effects Model",
                                  "Regularized Movie Effects Model",
                                  "Regularized Movie + User Effects Model",
                                  "Regularized Movie + User + Genre Effects Model"),
                       RMSE = c(naive_rmse, m_rmse, m_u_rmse, 
                                m_u_g_rmse, min(rmses_reg_m), 
                                min(rmses_reg_m_u),
                                min(rmses_reg_m_u_g)))
rmse_results %>% knitr::kable()

######################
# Chosen final model #
######################
# Estimate regularized average ranking for movie, user 
# and genre effects with optimal lambda
op_lambda <- lambdas[which.min(rmses_reg_m_u_g)]
movie_reg_bi <- edx %>% 
  group_by(movieId) %>% 
  summarize(b_i = sum(rating - mu)/(n()+op_lambda), n_i = n()) 
movie_reg_bu <- edx %>% 
  left_join(movie_reg_bi, by="movieId") %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - mu - b_i)/(n()+op_lambda), 
            n_i = n())
movie_reg_bg <- edx %>%
  left_join(movie_reg_bi, by="movieId") %>%
  left_join(movie_reg_bu, by='userId') %>%
  group_by(genres) %>%
  summarize(b_g = sum(rating - b_i - b_u - mu)/(n()+op_lambda), 
            n_i = n())

# Predict movie ratings in validation set
predicted_ratings_reg <- validation %>% 
  left_join(movie_reg_bi, by='movieId') %>%
  left_join(movie_reg_bu, by='userId') %>%
  left_join(movie_reg_bg, by='genres') %>%
  mutate(pred = mu + b_i + b_u + b_g) %>%
  pull(pred)

# Estimate RMSE in validation set
final_RMSE <- RMSE(validation$rating, predicted_ratings_reg)
final_RMSE
