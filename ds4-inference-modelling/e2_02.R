p <- 0.45
N <- 100
B <- 10000

set.seed(1)
take_sample <- function(p, N){
  x <- sample(c(0,1), size = N, replace = TRUE, prob = c(1-p, p))
  mean(x)
}

errors <- replicate(B, p - take_sample(p, N))
mean(errors)
