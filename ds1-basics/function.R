# example of defining a function to compute the average of a vector x
avg <- function(x){
  s <- sum(x)
  n <- length(x)
  s/n
}

x <- 1:100
c(avg(x), mean(x))
