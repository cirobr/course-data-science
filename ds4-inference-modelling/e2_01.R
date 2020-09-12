take_sample <- function(p, N){
  x <- sample(c(0,1), size = N, replace = TRUE, prob = c(1-p, p))
  mean(x)
}

set.seed(1)
p <- 0.45
N <- 100

take_sample(p, N)
