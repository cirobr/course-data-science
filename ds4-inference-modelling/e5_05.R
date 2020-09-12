# Load the 'dslabs' package and data contained in 'heights'
library(dplyr)
library(dslabs)
data(heights)

# Make a vector of heights from all males in the population
x <- heights %>% filter(sex == "Male") %>%
  .$height

mu <- mean(x)
mu
sd(x)

set.seed(1)
N <- 50
B <- 10000

res <- replicate(B, {
  X <- sample(x, N, replace = TRUE)
  se <- sd(X)/sqrt(N)
  interval<- c(mean(X) - qnorm(0.975)*se, mean(X) + qnorm(0.975)*se)
  between(mu, interval[1], interval[2])
})

mean(res)
