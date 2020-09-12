#Q2

results <- function(x){
  Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
  dat <- MASS::mvrnorm(x, c(69, 69), Sigma) %>%
    data.frame() %>% setNames(c("x", "y"))

  B <- 100
  res <- replicate(B, {
    
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
  
  c(mean = mean(res), sd = sd(res))
}

options(digits = 3)
n <- c(100, 500, 1000, 5000, 10000)

set.seed(1, sample.kind="Rounding") #if using R 3.6 or later
r <- map_df(n, results)
r <- cbind(n, r)
r
