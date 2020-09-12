library(tidyverse)
library(caret)

set.seed(1, sample.kind="Rounding") #if using R 3.6 or later
n <- 100
Sigma <- 9*matrix(c(1.0, 0.95, 0.95, 1.0), 2, 2)
dat <- MASS::mvrnorm(n = 100, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))

options(digits = 3)

set.seed(1, sample.kind="Rounding") #if using R 3.6 or later
B <- 100
results <- replicate(B, {
  
  # partition data on test and train sets
  test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
  test_set <- dat[test_index, ]
  train_set <- dat[-test_index, ]
  
  # generates a linear model for the above datasets
  fit <- lm(y ~ x, data = train_set)
  
  # calculates prediction for the model
  y_hat <- predict(fit, test_set)
  sqrt(mean((y_hat - test_set$y)^2))
})

c(mean(results), sd(results))
