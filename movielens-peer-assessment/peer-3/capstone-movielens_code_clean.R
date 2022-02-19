##########################################################################
##
#### Capstone project MovieLens 
#### Author: alex-stocker
##
##########################################################################

##########################################################################
##
#### Instructur code 
##
##########################################################################

##########################################################
# Create edx set, validation set (final hold-out test set)
##########################################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(data.table)

###############################################
# Download the MovieLens 10M dataset:
###############################################
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")

# if using R 4.0 or later:
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                            title = as.character(title),
                                            genres = as.character(genres))
movielens <- left_join(ratings, movies, by = "movieId")
movielens_backup <- movielens #backup to avoid downloading

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure that userId and movieId in validation set are also in edx set
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)
rm(ratings, movies, test_index, temp, movielens, removed)

##########################################################################
##
#### Data exploration 
##
##########################################################################

# I conducted a series of exercises to better understand the data 

# Q1: How many rows and columns are in the edx dataset?
nrow(edx)
ncol(edx)

# Q2: How many zeros were given as ratings in the edx dataset?
sum(edx$rating==0)

# Q2: How many threes were given as ratings in the edx dataset?
sum(edx$rating==3.0)
# edx %>% filter(rating==3.0) %>% nrow()

# Q3: How many different movies are in the edx dataset?
edx %>% summarize(n_movies = n_distinct(movieId))
# length(unique(edx$movieId))
# n_distinct(edx$movieId)

# Q4: How many different users are in the edx dataset?
edx %>% summarize(n_users = n_distinct(userId))
# n_distinct(edx$userId)

# Q5: How many movie ratings are in each of the following genres in the edx dataset?
nrow(edx %>% filter(str_detect(genres,"Drama")))
nrow(edx %>% filter(str_detect(genres,"Comedy")))
nrow(edx %>% filter(str_detect(genres,"Thriller")))
nrow(edx %>% filter(str_detect(genres,"Romance")))

# Q6: Which movie has the greatest number of ratings?
edx %>% group_by(title) %>% 
  summarise(number_of_ratings = n()) %>%
  arrange(desc(number_of_ratings)) %>% head(1)

#Q7: What are the five most given ratings in order from most to least?
edx %>% group_by(rating) %>% 
  summarise(number = n()) %>%
  arrange(desc(number)) %>% head(5)

#Q8 True or False: In general, half star ratings are less common than whole star ratings
edx %>%
  group_by(rating) %>%
  summarize(count = n()) %>%
  ggplot(aes(x = rating, y = count)) +
  geom_line()

# I also explored movie rating behavior 

# Some movies get more ratings than others
edx %>% 
  dplyr::count(movieId) %>% 
  ggplot(aes(n)) + 
  geom_histogram(bins = 30, color = "black") + 
  scale_x_log10() + 
  ggtitle("Movies")

# I found out that some users are more active rating movies than others
edx %>%
  dplyr::count(userId) %>% 
  ggplot(aes(n)) + 
  geom_histogram(bins = 30, color = "black") + 
  scale_x_log10() +
  ggtitle("Users")

##########################################################################
##
#### Data preparation 
##
##########################################################################

# I split the Edx data into a (a) training and (b) a test set
# The validation data (i.e. the final hold-out test set) will NOT be used 
# for training, developing, or selecting my algorithm and ONLY be used 
# for evaluating the RMSE of my final algorithm)

set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
test_index <- createDataPartition(y = edx$rating, times = 1, p = 0.1, list = FALSE)
train_set <- edx[-test_index,]
temp <- edx[test_index,]

# Make sure that userId and movieId in train_set are also in test_set
test_set <- temp %>% 
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId") %>% 
  semi_join(train_set, by = "title")

# Add rows removed from test set back into train set
removed <- anti_join(temp, test_set)
edx <- rbind(train_set, removed)
rm(ratings, movies, test_index, temp, movielens, removed)

# Function to compute the RMSE for vectors of ratings and their corresponding predictors
# to rate the quality of  models
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

##########################################################################
##
#### Model creation 
##
##########################################################################

##########################################################################
# Model 1: Mean rating model
##########################################################################

# The simplest recommendation system is to predict the same rating for all movies, 
# i.e. the mean of all ratings. 
mu <- mean(train_set$rating)
mu

##########################################################################
# Model 2: Movie Effect Model 
##########################################################################
# Ratings are subtracted by the mean for each rating that the movie received (bias)
# b_i is introduced as penalty term for movie effect
movie_avgs_norm <- train_set %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - mu))

qplot(b_i, data = movie_avgs_norm, bins = 10, color = I("black"))

predicted_ratings_b_i <- mu + test_set %>% 
  left_join(movie_avgs_norm, by='movieId') %>%
  pull(b_i)

##########################################################################
# Model 3: Movie Effect + User Effect Model 
##########################################################################
# Explore data for users that have rated more than 100 movies
# b_u is introduced as penalty term for movie effect
train_set %>% 
  group_by(userId) %>% 
  filter(n()>=100) %>%
  summarize(b_u = mean(rating)) %>% 
  ggplot(aes(b_u)) + 
  geom_histogram(bins = 30, color = "black")

user_avgs_norm <- train_set %>% 
  left_join(movie_avgs_norm, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))

qplot(b_u, data = user_avgs_norm, bins = 10, color = I("black"))

predicted_ratings_b_i_b_u <- test_set %>% 
  left_join(movie_avgs_norm, by='movieId') %>%
  left_join(user_avgs_norm, by='userId') %>%
  mutate(pred = mu + b_i + b_u) %>%
  pull(pred)

#### Further analysis on the results 

train_set %>% 
  left_join(movie_avgs_norm, by='movieId') %>%
  mutate(residual = rating - (mu + b_i)) %>%
  arrange(desc(abs(residual))) %>%  
  slice(1:15) %>% 
  pull(title)

# Connect movieId to movie title)
movie_titles <- train_set %>% 
  select(movieId, title) %>%
  distinct()

# 10 best movies according to our estimate
movie_avgs_norm %>% left_join(movie_titles, by="movieId") %>%
  arrange(desc(b_i)) %>% 
  slice(1:10)  %>% 
  pull(title)

# 10 worst movies
movie_avgs_norm %>% left_join(movie_titles, by="movieId") %>%
  arrange(b_i) %>% 
  slice(1:10)  %>% 
  pull(title)

# how often are these movies rated
train_set %>% count(movieId) %>% 
  left_join(movie_avgs_norm, by="movieId") %>%
  left_join(movie_titles, by="movieId") %>%
  arrange(desc(b_i)) %>% 
  slice(1:10) %>% 
  pull(n)

# ##########################################################################
# Model 4: Movie + User + Title Effect Model
# ##########################################################################
# "b_u" is introduced as penalty term for the title(-rating) effect.
# Average by title
title_avgs_norm <- train_set %>%
  left_join(movie_avgs_norm, by='movieId') %>%
  left_join(user_avgs_norm, by='userId') %>%
  group_by(title) %>%
  summarize(b_t = mean(rating - mu - b_i - b_u))

qplot(b_t, data = title_avgs_norm, bins = 10, color = I("black"))

predicted_ratings_b_i_b_u_b_t <- test_set %>%  
  left_join(movie_avgs_norm, by='movieId') %>%
  left_join(user_avgs_norm, by='userId') %>%
  left_join(title_avgs_norm, by='title') %>%
  mutate(pred = mu + b_i + b_u + b_t) %>%
  pull(pred)

##########################################################################
# Model 5: Regularized Movie + User Effect Model
##########################################################################
lambdas <- seq(3, 7, 0.25)

rmses <- sapply(lambdas, function(l){
    mu <- mean(train_set$rating)
    b_i <- train_set %>% 
     group_by(movieId) %>%
     summarize(b_i = sum(rating - mu)/(n()+l))
    b_u <- train_set %>% 
     left_join(b_i, by="movieId") %>%
     group_by(userId) %>%
     summarize(b_u = sum(rating - b_i - mu)/(n()+l))
    predicted_ratings <- test_set %>% 
     left_join(b_i, by = "movieId") %>%
     left_join(b_u, by = "userId") %>%
     mutate(pred = mu + b_i + b_u) %>%
    pull(pred)
  return(RMSE(predicted_ratings, test_set$rating))
})

min(rmses)

lambda <- lambdas[which.min(rmses)]
lambda

qplot(lambdas, rmses)

# Compute new predictions with optimal lambda

b_i <- train_set %>% 
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu)/(n()+lambda))

b_u <- train_set %>% 
  left_join(b_i, by="movieId") %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - b_i - mu)/(n()+lambda))

predicted_ratings_reg_1 <- test_set %>% 
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  mutate(pred = mu + b_i + b_u) %>%
  pull(pred)

##########################################################################
# Model 6: Regularized Movie + User + Title Effect Model
##########################################################################
lambdas <- seq(3, 7, 0.25)
rmses <- sapply(lambdas, function(l){
  mu <- mean(train_set$rating)
  b_i <- train_set %>%
   group_by(movieId) %>%
   summarize(b_i = sum(rating - mu)/(n()+lambda))
  b_u <- train_set %>% 
   left_join(b_i, by="movieId") %>%
   group_by(userId) %>%
   summarize(b_u = sum(rating - b_i - mu)/(n()+lambda))
  b_t <- train_set %>% 
   left_join(b_i, by="movieId") %>%
   left_join(b_u, by="userId") %>%
   group_by(title) %>%
   summarize(b_t = sum(rating - b_i - b_u - mu)/(n()+lambda))
  predicted_ratings_reg_2 <- test_set %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    left_join(b_t, by = "title") %>%
    mutate(pred = mu + b_i + b_u + b_t) %>%
    pull(pred)
   return(RMSE(predicted_ratings_reg_2, test_set$rating))
})

min(rmses)

lambda <- lambdas[which.min(rmses)]
lambda

qplot(lambdas, rmses)

# Compute new predictions with optimal lambda

b_i <- train_set %>% 
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu)/(n()+lambda))

b_u <- train_set %>% 
  left_join(b_i, by="movieId") %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - b_i - mu)/(n()+lambda))

b_t <- train_set %>% 
  left_join(b_i, by="movieId") %>%
  left_join(b_u, by="userId") %>%
  group_by(title) %>%
  summarize(b_t = sum(rating - b_i - b_u - mu)/(n()+lambda))

predicted_ratings_reg_2 <- test_set %>% 
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  left_join(b_t, by = "title") %>%
  mutate(pred = mu + b_i + b_u + b_t) %>%
  pull(pred)

##########################################################################
##
#### Model validation 
##
##########################################################################

##########################################################################
# Model validation with test set
##########################################################################

mu_RMSE <- RMSE(test_set$rating, mu)
mu_b_i_RMSE <- RMSE(test_set$rating, predicted_ratings_b_i)
mu_b_i_b_u_RMSE <- RMSE(test_set$rating,predicted_ratings_b_i_b_u)
mu_b_i_b_u_b_t_RMSE <- RMSE(test_set$rating,predicted_ratings_b_i_b_u_b_t)
regularized_RMSE_1 <- RMSE(test_set$rating,predicted_ratings_reg_1)
regularized_RMSE_2 <- RMSE(test_set$rating,predicted_ratings_reg_2)

# I build a results tibble for all methods using the test dataset
rmse_results <- data.frame(Method=c("Mean", 
                                    "+ Movie Effect", 
                                    "+ User Effect", 
                                    "+ Title Effect",
                                    "Regularized Movie + User Effect",
                                    "Regularized Movie + User + Title Effect"),
                           RMSE=c(mu_RMSE, 
                                  mu_b_i_RMSE, 
                                  mu_b_i_b_u_RMSE,
                                  mu_b_i_b_u_b_t_RMSE,
                                  regularized_RMSE_1,
                                  regularized_RMSE_2)
                          )
rmse_results

##########################################################################  
## Validation of the *best model* with the validation dataset
##########################################################################
predicted_ratings_val <- validation %>%
  left_join(movie_avgs_norm, by='movieId') %>%
  left_join(user_avgs_norm, by='userId') %>%
  left_join(title_avgs_norm, by='title') %>%
  mutate(b_i = ifelse(is.na(b_i), 0, b_i)) %>% # remove NAs
  mutate(b_u = ifelse(is.na(b_u), 0, b_u)) %>%
  mutate(b_t = ifelse(is.na(b_t), 0, b_t)) %>%
  mutate(pred = mu + b_i + b_u + b_t) %>%
  pull(pred)

RSME_val <- RMSE(validation$rating,predicted_ratings_val)
RSME_val



