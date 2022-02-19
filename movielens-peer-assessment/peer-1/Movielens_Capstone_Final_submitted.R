#Libraries
library(tidyverse)
library(janitor)
library(lubridate)
library(caret)

#Set graphical theme
theme_set(theme_bw())


## Downloading data and creating training and validation datasets ---------------------

###### Original script for getting the data is shown below --------


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

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")

# if using R 3.6 or earlier:
# movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
#                                            title = as.character(title),
#                                            genres = as.character(genres))
# if using R 4.0 or later:
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                           title = as.character(title),
                                           genres = as.character(genres))


movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
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

# Give the edx-dataset the same name as in the Rmd-file
dataset <- edx

#Dimensions
dim(dataset)
#Head of the data set
dataset %>% head()

#How many users and movies are there?
dataset %>% 
  summarise(n_users = n_distinct(userId),
            n_movies = n_distinct(movieId))

### Taking a subset of the `edx` data ------------------------------

#Setting the seed
set.seed(32)

# Amount of observation to be sampled
N <- 2000000
#Index of sampled observations
sample_index <- sample(x = 1:length(dataset$rating), size = N, replace = FALSE)
#Subset data
dataset <- dataset[sample_index,]

dim(dataset)


##### Modifying the data before analysis -----------------------


## Separating genres


## a function which takes a data.frame and separates genres-column based on vertical bar symbol "|"
separate_genres <- function(df){
  df <- df %>% 
    separate(col = genres, 
             into = paste0("genre", 1:8), 
             sep = "\\|", 
             extra = "warn", 
             fill = "right") %>%
    mutate(across(matches("genre"), 
                  function(x){if_else(is.na(x), "(no genres listed)", x)})
           ) # Turn NA into "(no genres listed)"
  return(df)
}

# separate the genres for the part of edx we use for data exploration
dataset <- separate_genres(dataset)

#Let's see what the data looks like
head(dataset)


### Getting the individual genre names ------------------------

### Extracting individual genres ------------------------

genre_df <- dataset %>% 
  select(-userId, -movieId, -rating, -timestamp, -title) %>% ## Leave individual genre columns
  pivot_longer(cols = everything(), 
               names_to = "genre_col", 
               values_to = "genre") # names "genre1", "genre2", values "Adventure" etc.
  
#get unique values for individual genres
indiv_genres <- unique(genre_df$genre)

#Let's remove the "genre_df" data.frame as do not need it anymore
rm("genre_df")



## Genres as individual columns ------------------------


# A function which determines in which genres a given movie belongs to
count_genres <- function(df){
  ### Let's create columns for all individual genres --------------------------
  ## Setting value as 1 if genre present, 0 otherwise
  
  #Ugly as hell but works
  #Could not think of a more elegant way to achieve this
  df <- df %>% # set values as 1 if genre name found from genre1-genre8-columns
    mutate(
      #Adventure
      Adventure = if_else((genre1 == "Adventure"|genre2 == "Adventure"|genre3 == "Adventure"|
                             genre4 == "Adventure"|genre5 == "Adventure"|genre6 == "Adventure"|
                             genre7 == "Adventure"|genre8 == "Adventure"),
                          true = 1, false = 0),
      #Children
      Children =  if_else((genre1 == "Children"|genre2 == "Children"|genre3 == "Children"|
                             genre4 == "Children"|genre5 == "Children"|genre6 == "Children"|
                             genre7 == "Children"|genre8 == "Children"),
                          true = 1, false = 0),
      #Fantasy
      Fantasy =  if_else((genre1 == "Fantasy"|genre2 == "Fantasy"|genre3 == "Fantasy"|
                            genre4 == "Fantasy"|genre5 == "Fantasy"|genre6 == "Fantasy"|
                            genre7 == "Fantasy"|genre8 == "Fantasy"),
                         true = 1, false = 0),
      #(no genres listed)
      `(no genres listed)` =  if_else((genre1 == "(no genres listed)"|genre2 == "(no genres listed)"|genre3 == "(no genres listed)"|
                                         genre4 == "(no genres listed)"|genre5 == "(no genres listed)"|genre6 == "(no genres listed)"|
                                         genre7 == "(no genres listed)"|genre8 == "(no genres listed)"),
                                      true = 1, false = 0),
      #Documentary
      Documentary =  if_else((genre1 == "Documentary"|genre2 == "Documentary"|genre3 == "Documentary"|
                                genre4 == "Documentary"|genre5 == "Documentary"|genre6 == "Documentary"|
                                genre7 == "Documentary"|genre8 == "Documentary"),
                             true = 1, false = 0),
      #Action
      Action =  if_else((genre1 == "Action"|genre2 == "Action"|genre3 == "Action"|
                           genre4 == "Action"|genre5 == "Action"|genre6 == "Action"|
                           genre7 == "Action"|genre8 == "Action"),
                        true = 1, false = 0),
      #Thriller
      Thriller =  if_else((genre1 == "Thriller"|genre2 == "Thriller"|genre3 == "Thriller"|
                             genre4 == "Thriller"|genre5 == "Thriller"|genre6 == "Thriller"|
                             genre7 == "Thriller"|genre8 == "Thriller"),
                          true = 1, false = 0),
      #Comedy
      Comedy =  if_else((genre1 == "Comedy"|genre2 == "Comedy"|genre3 == "Comedy"|
                           genre4 == "Comedy"|genre5 == "Comedy"|genre6 == "Comedy"|
                           genre7 == "Comedy"|genre8 == "Comedy"),
                        true = 1, false = 0),
      #Drama
      Drama =  if_else((genre1 == "Drama"|genre2 == "Drama"|genre3 == "Drama"|
                          genre4 == "Drama"|genre5 == "Drama"|genre6 == "Drama"|
                          genre7 == "Drama"|genre8 == "Drama"),
                       true = 1, false = 0),
      #War
      War =  if_else((genre1 == "War"|genre2 == "War"|genre3 == "War"|
                        genre4 == "War"|genre5 == "War"|genre6 == "War"|
                        genre7 == "War"|genre8 == "War"),
                     true = 1, false = 0),
      #Western
      Western =  if_else((genre1 == "Western"|genre2 == "Western"|genre3 == "Western"|
                            genre4 == "Western"|genre5 == "Western"|genre6 == "Western"|
                            genre7 == "Western"|genre8 == "Western"),
                         true = 1, false = 0),
      #Romance
      Romance =  if_else((genre1 == "Romance"|genre2 == "Romance"|genre3 == "Romance"|
                            genre4 == "Romance"|genre5 == "Romance"|genre6 == "Romance"|
                            genre7 == "Romance"|genre8 == "Romance"),
                         true = 1, false = 0),
      #Sci-Fi
      `Sci-Fi` =  if_else((genre1 == "Sci-Fi"|genre2 == "Sci-Fi"|genre3 == "Sci-Fi"|
                             genre4 == "Sci-Fi"|genre5 == "Sci-Fi"|genre6 == "Sci-Fi"|
                             genre7 == "Sci-Fi"|genre8 == "Sci-Fi"),
                          true = 1, false = 0),
      #Horror
      Horror =  if_else((genre1 == "Horror"|genre2 == "Horror"|genre3 == "Horror"|
                           genre4 == "Horror"|genre5 == "Horror"|genre6 == "Horror"|
                           genre7 == "Horror"|genre8 == "Horror"),
                        true = 1, false = 0),
      #Mystery
      Mystery =  if_else((genre1 == "Mystery"|genre2 == "Mystery"|genre3 == "Mystery"|
                            genre4 == "Mystery"|genre5 == "Mystery"|genre6 == "Mystery"|
                            genre7 == "Mystery"|genre8 == "Mystery"),
                         true = 1, false = 0),
      #Crime
      Crime =  if_else((genre1 == "Crime"|genre2 == "Crime"|genre3 == "Crime"|
                          genre4 == "Crime"|genre5 == "Crime"|genre6 == "Crime"|
                          genre7 == "Crime"|genre8 == "Crime"),
                       true = 1, false = 0),
      #Film-Noir
      `Film-Noir` =  if_else((genre1 == "Film-Noir"|genre2 == "Film-Noir"|genre3 == "Film-Noir"|
                                genre4 == "Film-Noir"|genre5 == "Film-Noir"|genre6 == "Film-Noir"|
                                genre7 == "Film-Noir"|genre8 == "Film-Noir"),
                             true = 1, false = 0),
      #Musical
      Musical =  if_else((genre1 == "Musical"|genre2 == "Musical"|genre3 == "Musical"|
                            genre4 == "Musical"|genre5 == "Musical"|genre6 == "Musical"|
                            genre7 == "Musical"|genre8 == "Musical"),
                         true = 1, false = 0),
      #Animation
      Animation =  if_else((genre1 == "Animation"|genre2 == "Animation"|genre3 == "Animation"|
                              genre4 == "Animation"|genre5 == "Animation"|genre6 == "Animation"|
                              genre7 == "Animation"|genre8 == "Animation"),
                           true = 1, false = 0),
      #IMAX
      IMAX =  if_else((genre1 == "IMAX"|genre2 == "IMAX"|genre3 == "IMAX"|
                         genre4 == "IMAX"|genre5 == "IMAX"|genre6 == "IMAX"|
                         genre7 == "IMAX"|genre8 == "IMAX"),
                      true = 1, false = 0)
    ) %>% 
    select(-contains(as.character(1:8))) %>% # Lets get rid of genre1...genre8-columns
    janitor::clean_names()  #Let's clean column names
  
  return(df)
}


### Modified dataframe -------------------------

dataset <- count_genres(dataset)
#Inspecting the dataset
head(dataset)


## Extracting year from the title -------------------

## A function for extracting the movie release year from the title column ------------------
extract_year <- function(df){
  
  df <- df %>% 
    mutate(year = str_extract(string = title, pattern = "\\(\\d{4}\\)")) %>% #extract year in brackets
    mutate(year = gsub(pattern = "\\(", replacement = "", x = year)) %>% # remove brackets
    mutate(year = gsub(pattern = "\\)", replacement = "", x = year)) %>% # remove brackets
    mutate(year = as.numeric(year)) #make numeric
  
  return(df)
} 

dataset <- extract_year(dataset)
#Let's see if the function works
dataset %>% select(title, year) %>% head()

#Range of movie release years
dataset %>% pull(year) %>% range()



##### Exploratory data analysis ----------------------------------------------


## Splitting the data

# Index for data partitioning
set.seed(32)
trainIndex <- createDataPartition(dataset$rating,
                                  p = 0.8,
                                  list = FALSE,
                                  times = 1)

#Train test split
dataset_train <- dataset[trainIndex,]
dataset_test <- dataset[-trainIndex,]


#Making sure test set does not include users and movies not available in the train set
dataset_test <- dataset_test %>%
  semi_join(dataset_train, by = "movie_id") %>%
  semi_join(dataset_train, by = "user_id")

# How large is our train data
dim(dataset_train)
# How large is our test data
dim(dataset_test)


## Effect of genre ---------------------------------

indiv_genres <- names(dataset)[-(1:5)] #The names for individual genres
# Let's get rid of the newly created "year" column
(indiv_genres <- indiv_genres[indiv_genres != "year"]) 

# Let's summarise mean and standard deviation of ratings
# as well as number of observations for all genre columns
dataset_train %>% 
  pivot_longer(cols = all_of(indiv_genres), 
               names_to = "genre", 
               values_to = "value") %>% 
  group_by(genre, value) %>% 
  summarise(avg = mean(rating), sd = sd(rating), n = n())

#Average rating for all movies
average_rating <- mean(dataset_train$rating)

# Plot average rating for individual genres
dataset_train %>% 
  pivot_longer(cols = all_of(indiv_genres), 
               names_to = "genre", 
               values_to = "value") %>% 
  group_by(genre, value) %>% 
  summarise(avg = mean(rating), sd = sd(rating), n = n()) %>% 
  filter(value == 1) %>% 
  ggplot(aes(x = genre, y = avg)) +
  geom_pointrange(aes(ymin = (avg - sd), ymax = (avg+sd))) +
  geom_hline(yintercept = average_rating, lty = 2, color = "red") +
  geom_text(aes( y = 2.35, label = paste("n =", n))) +
  coord_flip() + 
  ggtitle("Effect of genre on rating", 
          subtitle = "Dashed red line indicates average rating for all movies") +
  labs(y = "Average rating")



## Effect of year of release -----------------------


#Does the release year have an effect on the rating distribution
dataset_train %>% 
  group_by(year) %>%
  summarise(median = median(rating),
            mean = mean(rating),
            sd = sd(rating)) %>%
  ggplot(aes(x = year, y = mean)) +
  geom_point(aes(y = median), alpha = 0.5) +
  geom_pointrange(aes(ymin = (mean - sd), ymax = (mean + sd)), color = "red", alpha = 0.7) +
  geom_smooth(method = "loess") +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(y = "Mean and median rating") +
  ggtitle("Effect of the year of movie release to average and median rating")

#ratings per year of release
dataset_train %>% ggplot(aes(x= year)) +
  geom_histogram(color = "black", fill = "red", binwidth = 1) +
  ggtitle("Number of ratings for movies per year of release")


## Effect of rating time --------------------


# How do rating dates spread out
dataset_train %>%  mutate(rating_date = as_datetime(timestamp)) %>%  
   mutate(rating_date = round_date(rating_date, unit ="day")) %>% 
  ggplot(aes(x = rating_date)) +
  geom_histogram(color = "black", fill = "red") +
  ggtitle("Spread of ratings by rating date")

# Function for extracting rating year and month from timestamp
calculate_rating_time <- function(df){
  df_mod <- df %>% 
    mutate(rating_date = as_datetime(timestamp)) %>%  
    mutate(rating_date = round_date(rating_date, unit ="day")) %>% 
    mutate(rating_year = year(rating_date),
           rating_month = month(rating_date))
  return(df_mod)
}

# Do the average ratings vary by rating year?
# Plot rating_year vs rating
dataset_train %>% calculate_rating_time() %>% 
  group_by(rating_year) %>% 
  summarise(avg_rating = mean(rating), sd_rating = sd(rating)) %>% 
  ggplot(aes(x = rating_year, y = avg_rating)) +
  geom_pointrange(aes(ymin = average_rating - sd_rating,
                      ymax = average_rating + sd_rating)) +
  geom_smooth()

# Do the average ratings vary by month
dataset_train %>% calculate_rating_time() %>% 
  group_by(rating_month) %>% 
  summarise(avg_rating = mean(rating), sd_rating = sd(rating)) %>% 
  ggplot(aes(x = rating_month, y = avg_rating)) +
  geom_pointrange(aes(ymin = average_rating - sd_rating,
                      ymax = average_rating + sd_rating)) +
  geom_smooth()


### Model training ------------------------------


## Model evaluation -----------------------------

# A function for calculating RMSE
calculate_RMSE <- function(y, y_hat){
  sqrt(mean((y_hat - y)^2))
}


## Simple model with average rating ------------------

#mean for training dataset
mu <- mean(dataset_train$rating)


### Evaluating the baseline for model performance

#Evaluating the performance of the model with test data
model1 <- calculate_RMSE(y = dataset_test$rating, y_hat = mu)

# Let's collect the performance of different models in this tibble
rmse_results <- tibble(method = "Just the average", RMSE = model1)

rmse_results %>% knitr::kable()


## Movie effect ---------------

# Effect of movie on the rating 
movie_effect <- dataset_train %>% 
  group_by(movie_id) %>% 
  summarise(b_m = mean(rating - mu))

#Let's inspect the movie effects
head(movie_effect)
#How do the values spread out
movie_effect %>% 
  ggplot(aes(x = b_m)) +
  geom_histogram(fill = "red", color = "black") +
  ggtitle("Histogram of movie effects")


# predictions
predicted_ratings <- mu + dataset_test %>% 
  left_join(movie_effect, by='movie_id') %>%
  pull(b_m)

#calculate RMSE for model 2
model2 <- calculate_RMSE(dataset_test$rating, predicted_ratings)

#Compare performance
rmse_results <- rmse_results %>% bind_rows(tibble(method = "Movie effect", RMSE = model2))

rmse_results %>% knitr::kable()

## User effect -------------------------

#Let's estimate the user effect
user_effect <- dataset_train %>% 
  left_join(movie_effect, by = "movie_id") %>% 
  group_by(user_id) %>% 
  summarise(b_u = mean(rating - mu - b_m))

#Inspecting the element
head(user_effect)
#How do the values spread out
user_effect %>% 
  ggplot(aes(x = b_u)) +
  geom_histogram(fill = "red", color = "black") +
  ggtitle("Histogram of user effects")

#Predictions with user effect
predicted_ratings <- dataset_test %>% 
  left_join(movie_effect, by='movie_id') %>%
  left_join(user_effect, by='user_id') %>%
  mutate(pred = mu + b_m + b_u) %>%
  pull(pred)

#Calculate RMSE for model 3
model3 <- calculate_RMSE(dataset_test$rating, predicted_ratings)

#Compare performance
rmse_results <- rmse_results %>% bind_rows(tibble(method = "Movie+user effect", RMSE = model3))

rmse_results %>% knitr::kable()


## Year effect -----------------------------------

# Calculate effect of release year
year_effect <- dataset_train %>% 
  left_join(movie_effect, by = "movie_id") %>% 
  left_join(user_effect, by = "user_id") %>%
  group_by(year) %>% 
  summarise(b_y = mean(rating - mu - b_m - b_u))

#Inspecting the element
head(year_effect)
#How do the values spread out
year_effect %>% 
  ggplot(aes(x = b_y)) +
  geom_histogram(fill = "red", color = "black") +
  ggtitle("Histogram of release year effects")


#Predictions with movie + user and release year effect
predicted_ratings <- dataset_test %>% 
  left_join(movie_effect, by='movie_id') %>%
  left_join(user_effect, by='user_id') %>%
  left_join(year_effect, by='year') %>%
  mutate(pred = mu + b_m  + b_u + b_y) %>%
  pull(pred)

#Calculate RMSE for model 4
(model4 <- calculate_RMSE(dataset_test$rating, predicted_ratings))

#Compare performance
rmse_results <- rmse_results %>% bind_rows(tibble(method = "Movie+user+release year effect", RMSE = model4))

rmse_results %>% knitr::kable()


## Rating time effect -----------------------------

# Calculate effect of rating year and month
rating_year_effect <- dataset_train %>% 
  left_join(movie_effect, by = "movie_id") %>% 
  left_join(user_effect, by = "user_id") %>%
  left_join(year_effect, by = "year") %>%
  calculate_rating_time() %>%
  group_by(rating_year, rating_month) %>% 
  summarise(b_rym = mean(rating - mu - b_m - b_u - b_y))

#Inspecting the element
head(rating_year_effect)
#How do the values spread out
rating_year_effect %>% 
  ggplot(aes(x = b_rym)) +
  geom_histogram(fill = "red", color = "black") +
  ggtitle("Histogram of rating time effects")

#Predictions with movie + user and release year effect
predicted_ratings <- dataset_test %>% 
  left_join(movie_effect, by='movie_id') %>%
  left_join(user_effect, by='user_id') %>%
  left_join(year_effect, by='year') %>%
  calculate_rating_time() %>%
  left_join(rating_year_effect, by = c('rating_year', 'rating_month')) %>% 
  mutate(pred = mu + b_m  + b_u + b_y + b_rym) %>%
  pull(pred)

#Calculate RMSE for model 5
model5 <- calculate_RMSE(dataset_test$rating, predicted_ratings)

#Compare performance
rmse_results <- rmse_results %>% bind_rows(tibble(method = "Movie+user+release year+rating time effect", RMSE = model5))

rmse_results %>% knitr::kable()


## Genre effect --------------------------------

#The effect of individual genres
genre_effect <- dataset_train  %>% 
  left_join(movie_effect, by = "movie_id") %>% 
  left_join(user_effect, by = "user_id") %>%
  left_join(year_effect, by = "year") %>%
  calculate_rating_time() %>%
  left_join(rating_year_effect, by = c('rating_year', 'rating_month')) %>% 
  pivot_longer(cols = all_of(indiv_genres), names_to = "genre", values_to = "value") %>% 
  filter(value == 1) %>% 
  group_by(genre) %>% 
  summarise(b_g = mean(rating - mu - b_m - b_u - b_y - b_rym))

#Effect of individual genres
genre_effect

# A function for calculating the summed total genre combination effect
#Individual genre weights will be set as 0 if genre not present
calculate_genre_effect <- function(df){
  df_with_genre_effect <- df %>% 
    mutate(b_g = 
             adventure * genre_effect$b_g[genre_effect$genre == "adventure"] +
             children * genre_effect$b_g[genre_effect$genre == "children"] +
             fantasy * genre_effect$b_g[genre_effect$genre == "fantasy"] +
             documentary * genre_effect$b_g[genre_effect$genre == "documentary"] +
             action * genre_effect$b_g[genre_effect$genre == "action"] +
             thriller * genre_effect$b_g[genre_effect$genre == "thriller"] +
             comedy * genre_effect$b_g[genre_effect$genre == "comedy"] +
             drama * genre_effect$b_g[genre_effect$genre == "drama"] +
             war * genre_effect$b_g[genre_effect$genre == "war"] +
             western * genre_effect$b_g[genre_effect$genre == "western"] +
             romance * genre_effect$b_g[genre_effect$genre == "romance"] +
             sci_fi * genre_effect$b_g[genre_effect$genre == "sci_fi"] +
             horror * genre_effect$b_g[genre_effect$genre == "horror"] +
             mystery * genre_effect$b_g[genre_effect$genre == "mystery"] +
             crime* genre_effect$b_g[genre_effect$genre == "crime"] +
             film_noir * genre_effect$b_g[genre_effect$genre == "film_noir"] +
             musical * genre_effect$b_g[genre_effect$genre == "musical"] +
             animation * genre_effect$b_g[genre_effect$genre == "animation"] +
             imax * genre_effect$b_g[genre_effect$genre == "imax"]
    )
  
  return(df_with_genre_effect)
}



#Predictions with movie + user + release year and genre effect
predicted_ratings <- dataset_test %>% 
  left_join(movie_effect, by='movie_id') %>%
  left_join(user_effect, by='user_id') %>%
  left_join(year_effect, by='year') %>%
  calculate_rating_time() %>%
  left_join(rating_year_effect, by = c('rating_year', 'rating_month')) %>% 
  calculate_genre_effect() %>% 
  mutate(pred = mu + b_m + b_u + b_y + b_rym + b_g) %>%
  pull(pred)


#Calculate RMSE for model 6
model6 <- calculate_RMSE(dataset_test$rating, predicted_ratings)

#Compare performance
rmse_results <- rmse_results %>% 
  bind_rows(tibble(method = "Movie+user+release year+rating time+genre effect", RMSE = model6))

rmse_results %>% knitr::kable()


## Movie neighbors model --------------------------------

movie_neighbor_effects <- dataset_train %>% 
  left_join(movie_effect, by='movie_id') %>%
  left_join(user_effect, by='user_id') %>%
  left_join(year_effect, by='year') %>%
  calculate_rating_time() %>%
  left_join(rating_year_effect, by = c('rating_year', 'rating_month')) %>% 
  calculate_genre_effect() %>%
  group_by(b_g) %>% 
  summarise(b_mn = mean(rating - mu - b_m - b_u - b_y - b_rym - b_g))

#Inspecting the element
head(movie_neighbor_effects)
#How do the values spread out
movie_neighbor_effects %>% 
  ggplot(aes(x = b_mn)) +
  geom_histogram(fill = "red", color = "black") +
  ggtitle("Histogram of movie neighbor effects")

#Predictions with movie + user + release year + genre effect and movie neighbor model
predicted_ratings <- dataset_test %>% 
  left_join(movie_effect, by='movie_id') %>%
  left_join(user_effect, by='user_id') %>%
  left_join(year_effect, by='year') %>%
  calculate_rating_time() %>%
  left_join(rating_year_effect, by = c('rating_year', 'rating_month')) %>% 
  calculate_genre_effect() %>% 
  left_join(movie_neighbor_effects, by='b_g') %>%
  mutate(pred = mu + b_m + b_u + b_y + b_rym + b_g + b_mn) %>%
  pull(pred)

#Calculate RMSE for model 7
model7 <- calculate_RMSE(dataset_test$rating, predicted_ratings)

#Compare performance
rmse_results <- rmse_results %>% bind_rows(tibble(method = "Movie+user+(r&r)year+genre+movie neighbor effect", RMSE = model7))

rmse_results %>% knitr::kable()


## Regularization ----------------------------------

# Different regularization parameter values to try
lambdas <- seq(0, 10, 0.5)

# Ridge regression type of regularization (L2)
rmses <- sapply(lambdas, function(l){
  mu <- mean(dataset_train$rating)
  
  ## Effects --------------------
  
  #movie effect
  b_m <- dataset_train %>%
    group_by(movie_id) %>%
    summarize(b_m = sum(rating - mu)/(n()+l))
  
  #user effect
  b_u <- dataset_train %>%
    left_join(b_m, by="movie_id") %>%
    group_by(user_id) %>%
    summarize(b_u = sum(rating - mu - b_m)/(n()+l))
  
  # Calculate effect of release year
  b_y <- dataset_train %>% 
    left_join(b_m, by = "movie_id") %>% 
    left_join(b_u, by = "user_id") %>%
    group_by(year) %>% 
    summarise(b_y = sum(rating - mu - b_m - b_u)/(n()+l))
  
  # Calculate effect of rating year and month
  b_rym <- dataset_train %>% 
    left_join(b_m, by = "movie_id") %>% 
    left_join(b_u, by = "user_id") %>%
    left_join(b_y, by = "year") %>%
    calculate_rating_time() %>%
    group_by(rating_year, rating_month) %>% 
    summarise(b_rym = sum(rating - mu - b_m - b_u - b_y)/(n() + l))
  
  # Calculate effect of genre
  genre_effect <- dataset_train  %>% 
    left_join(b_m, by = "movie_id") %>% 
    left_join(b_u, by = "user_id") %>%
    left_join(b_y, by = "year") %>%
    calculate_rating_time() %>%
    left_join(b_rym, by = c('rating_year', 'rating_month')) %>% 
    pivot_longer(cols = all_of(indiv_genres), names_to = "genre", values_to = "value") %>% 
    filter(value == 1) %>% 
    group_by(genre) %>% 
    summarise(b_g = sum(rating - mu - b_m - b_u - b_y - b_rym)/(n()+l))
  
  # Calculate effect of movie neighbors == similar in terms of genre spread
  b_mn <- dataset_train %>% 
    left_join(b_m, by='movie_id') %>%
    left_join(b_u, by='user_id') %>%
    left_join(b_y, by='year') %>%
    calculate_rating_time() %>%
    left_join(b_rym, by = c('rating_year', 'rating_month')) %>% 
    calculate_genre_effect() %>%
    group_by(b_g) %>% 
    summarise(b_mn = sum(rating - mu - b_m - b_u - b_y - b_rym - b_g)/(n()+l))
  
  ## Models --------------------
  
  #Movie + user effect
  predicted_ratings <- dataset_test %>%
    left_join(b_m, by = "movie_id") %>%
    left_join(b_u, by = "user_id") %>%
    mutate(pred = mu + b_m + b_u) %>%
    pull(pred)
  
  #performance of model
  RMSE1 <- calculate_RMSE(predicted_ratings, dataset_test$rating)
  
  #Movie + user + release year effect
  predicted_ratings <- dataset_test %>%
    left_join(b_m, by = "movie_id") %>%
    left_join(b_u, by = "user_id") %>%
    left_join(b_y, by = "year") %>% 
    mutate(pred = mu + b_m + b_u + b_y) %>%
    pull(pred)
  
  #performance of model
  RMSE2 <- calculate_RMSE(predicted_ratings, dataset_test$rating)
  
  
  #Predictions with movie + user + release year and rating time effect
  predicted_ratings <- dataset_test %>% 
    left_join(b_m, by='movie_id') %>%
    left_join(b_u, by='user_id') %>%
    left_join(b_y, by='year') %>%
    calculate_rating_time() %>%
    left_join(b_rym, by = c('rating_year', 'rating_month')) %>% 
    mutate(pred = mu + b_m  + b_u + b_y + b_rym) %>%
    pull(pred)
  
  #performance of model
  RMSE3 <- calculate_RMSE(predicted_ratings, dataset_test$rating)
  
  #Predictions with movie + user + release year and genre effect
  predicted_ratings <- dataset_test %>% 
    left_join(b_m, by='movie_id') %>%
    left_join(b_u, by='user_id') %>%
    left_join(b_y, by='year') %>%
    calculate_rating_time() %>%
    left_join(b_rym, by = c('rating_year', 'rating_month')) %>% 
    calculate_genre_effect() %>% 
    mutate(pred = mu + b_m + b_u + b_y + b_rym + b_g) %>%
    pull(pred)
  
  #performance of model
  RMSE4 <- calculate_RMSE(predicted_ratings, dataset_test$rating)
  
  #Predictions with movie + user + releas year + genre effect and movie neighbor model
  predicted_ratings <- dataset_test %>% 
    left_join(b_m, by='movie_id') %>%
    left_join(b_u, by='user_id') %>%
    left_join(b_y, by='year') %>%
    calculate_rating_time() %>%
    left_join(b_rym, by = c('rating_year', 'rating_month')) %>% 
    calculate_genre_effect() %>% 
    left_join(b_mn, by='b_g') %>%
    mutate(pred = mu + b_m + b_u + b_y + b_rym + b_g + b_mn) %>%
    pull(pred)
  
  #performance of model
  RMSE5 <- calculate_RMSE(predicted_ratings, dataset_test$rating)
  
  ## Results for regularized models -----------------------
  
  #Combine models
  RMSEs <- cbind(RMSE1, RMSE2, RMSE3, RMSE4, RMSE5)
  
  return(RMSEs)
})


# Regularized results for models
results <- tibble(lambda = lambdas, 
                  model1 = rmses[1,], #Average + Movie + user effect
                  model2 = rmses[2,], # + Release year
                  model3 = rmses[3,], # + Rating year and month
                  model4 = rmses[4,], # + genre effect
                  model5 = rmses[5,]) # + movie neighbor effect

#Different models used in regularization
model_desc <- tribble(
  ~model, ~description,
  "model1", "Average + Movie+user effect",
  "model2", "AMU + Release year", #AMU = Average, Movie, User
  "model3", "AMUY + Rating year&month", #Y = year
  "model4", "AMUYT + genre effect", # T = time (of rating)
  "model5", "AMUYTG + movie neighbor effect" # G = Genre
)

#Visualize regularization effect on different models
results %>% pivot_longer(cols = -lambda, names_to = "model", values_to = "RMSE") %>% 
  left_join(model_desc, by = "model") %>% 
  ggplot(aes(x = lambda, y = RMSE, color = description)) +
  geom_point() +
  ggtitle("Regularized model performance")

#Find minimum rmse
min_rmse <- results %>% 
  pivot_longer(cols = -lambda, names_to = "model", values_to = "RMSE") %>%
  pull(RMSE) %>% 
  which.min()

#BEST RESULT
results %>% 
  pivot_longer(cols = -lambda, names_to = "model", values_to = "RMSE") %>%
  left_join(model_desc, by = "model") %>%
  slice(min_rmse)

# Saving the lambda for final model training
tuning_parameter <- results %>% 
  pivot_longer(cols = -lambda, names_to = "model", values_to = "RMSE") %>%
  left_join(model_desc, by = "model") %>%
  slice(min_rmse) %>% pull(lambda)


## Training the final model with the entire training data -----------------------------

# training the final model with training and test data

#Let's free up memory first
rm("dataset_test", "dataset_train") #remove train and test
gc() #Collect garbage

## Parameter setting -----------
# determining model parameters with entire train and test data
mu <- mean(dataset$rating)

## Effects --------------------

#movie effect
b_m <- dataset %>%
  group_by(movie_id) %>%
  summarize(b_m = sum(rating - mu)/(n()+tuning_parameter))

#user effect
b_u <- dataset %>%
  left_join(b_m, by="movie_id") %>%
  group_by(user_id) %>%
  summarize(b_u = sum(rating - mu - b_m)/(n()+tuning_parameter))

# Calculate effect of release year
b_y <- dataset %>% 
  left_join(b_m, by = "movie_id") %>% 
  left_join(b_u, by = "user_id") %>%
  group_by(year) %>% 
  summarise(b_y = sum(rating - mu - b_m - b_u)/(n()+tuning_parameter))

# Calculate effect of rating year and month
b_rym <- dataset %>% 
  left_join(b_m, by = "movie_id") %>% 
  left_join(b_u, by = "user_id") %>%
  left_join(b_y, by = "year") %>%
  calculate_rating_time() %>%
  group_by(rating_year, rating_month) %>%  
  summarise(b_rym = sum(rating - mu - b_m - b_u - b_y)/(n() + tuning_parameter))

# Calculate effect of genre
genre_effect <- dataset  %>% 
  left_join(b_m, by = "movie_id") %>% 
  left_join(b_u, by = "user_id") %>%
  left_join(b_y, by = "year") %>%
  calculate_rating_time() %>%
  left_join(b_rym, by = c('rating_year', 'rating_month')) %>% 
  pivot_longer(cols = all_of(indiv_genres), names_to = "genre", values_to = "value") %>% 
  filter(value == 1) %>% 
  group_by(genre) %>% 
  summarise(b_g = sum(rating - mu - b_m - b_u - b_y - b_rym)/(n()+tuning_parameter))

# Calculate effect of movie neighbors == similar in terms of genre spread
b_mn <- dataset %>% 
  left_join(b_m, by='movie_id') %>%
  left_join(b_u, by='user_id') %>%
  left_join(b_y, by='year') %>%
  calculate_rating_time() %>%
  left_join(b_rym, by = c('rating_year', 'rating_month')) %>% 
  calculate_genre_effect() %>%
  group_by(b_g) %>% 
  summarise(b_mn = sum(rating - mu - b_m - b_u - b_y - b_rym - b_g)/(n()+tuning_parameter))

## Model predictions -----------
# A function for making predictions with the final model
predict_ratings <- function(df){
  ##Predictions with movie + user + release year + genre effect and movie neighbor model
  df <- df %>% 
    left_join(b_m, by='movie_id') %>%
    left_join(b_u, by='user_id') %>%
    left_join(b_y, by='year') %>%
    calculate_rating_time() %>%
    left_join(b_rym, by = c('rating_year', 'rating_month')) %>% 
    calculate_genre_effect() %>% 
    left_join(b_mn, by='b_g') %>%
    mutate(pred = mu + b_m + b_u + b_y + b_rym + b_g + b_mn)
  
  ## Let's impute the average for predictions that turn out NA
  ## This may happen since we did not use the entire edx data for training
  ## so some user_id & movie_id values might not be included
  predicted_ratings <- df %>%   
    mutate(pred = if_else(is.na(pred), # IF prediction is NA
                          true = mu, # RETURN the train average
                          false = pred)) %>%  # ELSE return the prediction
    pull(pred)
  
  return(predicted_ratings)
}


## Calculating training error (RMSE) for the final model -------------------------------

calculate_RMSE(dataset$rating, predict_ratings(dataset))


dataset %>% head(n = 1000) %>% 
  mutate(pred = predict_ratings(head(dataset, n = 1000))) %>% 
  ggplot(aes(x = pred, y = rating)) +
  geom_point() +
  geom_abline(slope = 1, color = "red") +
  labs(x = "Predicted rating", y = "True rating") +
  ggtitle("Comparison of 1000 predicted and true ratings",
          subtitle = "Predictions on the training data")


# Final model performance with validation dataset ------------------------------


# Loading the validation data set from local folder
#validation <- readRDS(file = "MovielensData/validation.Rds") ### Not needed here (no local Rds-copy)
dim(validation)

# We need to do the same modifications for the validation data we did for the training data set
validation <- validation %>% 
  separate_genres() %>% 
  count_genres() %>% 
  extract_year()

dim(validation)

# RMSE for predictions made for validation dataset
(Final_model_RMSE <- calculate_RMSE(validation$rating, predict_ratings(validation)))
