library(tidyverse)
library(caret)

# example
set.seed(2, sample.kind="Rounding") #if you are using R 3.6 or later
make_data <- function(n = 1000,
                      p = 0.5, 
                      mu_0 = 0,
                      mu_1 = 2, 
                      sigma_0 = 1,
                      sigma_1 = 1){
  
  y <- rbinom(n, 1, p)
  f_0 <- rnorm(n, mu_0, sigma_0)
  f_1 <- rnorm(n, mu_1, sigma_1)
  x <- ifelse(y == 1, f_1, f_0)
  
  test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
  
  list(train = data.frame(x = x, y = as.factor(y)) %>% slice(-test_index),
       test = data.frame(x = x, y = as.factor(y)) %>% slice(test_index))
}

dat <- make_data()
dat$train %>% ggplot(aes(x, color = y)) + geom_density()


#Q1
make_data <- function(mu_1,
                      n = 1000,
                      p = 0.5, 
                      mu_0 = 0,
                      #mu_1 = 2, 
                      sigma_0 = 1,
                      sigma_1 = 1){
  
  y <- rbinom(n, 1, p)
  f_0 <- rnorm(n, mu_0, sigma_0)
  f_1 <- rnorm(n, mu_1, sigma_1)
  x <- ifelse(y == 1, f_1, f_0)
  
  test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
  
  list(train = data.frame(x = x, y = as.factor(y)) %>% slice(-test_index),
       test = data.frame(x = x, y = as.factor(y)) %>% slice(test_index))
}

result <- function(i){
  # generates data
  dat1 <- dat[,i]
  
  # partition data on test and train sets
  train_set <- dat1$train
  test_set <- dat1$test
  
  # generates a logistic model for the above datasets
  glm_fit <- glm(y ~ x, data = train_set, family = "binomial")
  
  # calculates prediction for the model
  p_hat_logit <- predict(glm_fit, newdata = test_set, type = "response")
  p_hat <- ifelse(p_hat_logit > 0.5, 1, 0) %>% factor()
  
  # calculates accuracy for the model
  mean(test_set$y == p_hat)
}

B <- 25
mu_1 <- seq(0, 3, len=B)
set.seed(1, sample.kind="Rounding") #if you are using R 3.6 or later
dat <- sapply(mu_1, make_data)

i <- seq(1:B)
res <- sapply(i, result)

df <- data.frame(mu_1, res)
df %>% ggplot(aes(mu_1, res)) +
  geom_point()