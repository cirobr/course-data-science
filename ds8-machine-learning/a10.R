#Q8
library(tidyverse)
library(caret)
options(digits = 3)

set.seed(1, sample.kind="Rounding") #if using R 3.6 or later
Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.95, 0.75, 0.95, 1.0), 3, 3)
dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
  data.frame() %>% setNames(c("y", "x_1", "x_2"))

set.seed(1, sample.kind="Rounding") #if using R 3.6 or later

# partition data on test and train sets
test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
test_set <- dat[test_index, ]
train_set <- dat[-test_index, ]

# generates a linear model for the above datasets
fit1 <- lm(y ~ x_1, data = train_set)
fit2 <- lm(y ~ x_2, data = train_set)
fit3 <- lm(y ~ x_1 + x_2, data = train_set)

# calculates prediction for the model
y_hat1 <- predict(fit1, test_set)
sqrt(mean((y_hat1 - test_set$y)^2))

y_hat2 <- predict(fit2, test_set)
sqrt(mean((y_hat2 - test_set$y)^2))

y_hat3 <- predict(fit3, test_set)
sqrt(mean((y_hat3 - test_set$y)^2))
