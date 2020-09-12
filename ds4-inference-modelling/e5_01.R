# Load the 'dslabs' package and data contained in 'heights'
library(dplyr)
library(dslabs)
data(heights)

# Make a vector of heights from all males in the population
x <- heights %>% filter(sex == "Male") %>%
  .$height

mean(x)
sd(x)

set.seed(1)
N <- 50
X <- sample(x, N, replace = TRUE)

mean(X)
sd(X)

se <- sd(X)/sqrt(N)
se

qnorm(0.975)
ci <- c(mean(X) - qnorm(0.975)*se, mean(X) + qnorm(0.975)*se)
