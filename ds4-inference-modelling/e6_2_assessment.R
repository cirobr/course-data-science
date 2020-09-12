#t-distribution vs normal distribution

#Exercise 01
# Calculate the probability of seeing t-distributed random variables being more than 2 in absolute value when 'df = 3'.
sd <- 2
df <- 3
p_upper <- 1 - pt(sd, df)
p_lower <- pt(-sd, df)
p <- p_upper + p_lower
p



#Exercise 02
# Generate a vector 'df' that contains a sequence of numbers from 3 to 50
df <- seq(3, 50)

# Make a function called 'pt_func' that calculates the probability that a value is more than |2| for any degrees of freedom 
pt_func <- function(df_x){
  sd <- 2
  p_upper <- 1 - pt(sd, df_x)
  p_lower <- pt(-sd, df_x)
  p_upper + p_lower
}

# Generate a vector 'probs' that uses the `pt_func` function to calculate the probabilities
probs <- sapply(df, pt_func)

# Plot 'df' on the x-axis and 'probs' on the y-axis
plot(df, probs)



#Exercise 03
# Load the neccessary libraries and data
library(dslabs)
library(dplyr)
data(heights)

# Use the sample code to generate 'x', a vector of male heights
x <- heights %>% filter(sex == "Male") %>%
  .$height

# Create variables for the mean height 'mu', the sample size 'N', and the number of times the simulation should run 'B'
mu <- mean(x)
N <- 15
B <- 10000

# Use the `set.seed` function to make sure your answer matches the expected result after random sampling
set.seed(1)

# Generate a logical vector 'res' that contains the results of the simulations
res <- replicate(B, {
  X <- sample(x, N, replace = TRUE)
  se <- sd(X)/sqrt(N)
  interval<- c(mean(X) - qnorm(0.975) * se, mean(X) + qnorm(0.975) * se)
  between(mu, interval[1], interval[2])
})

# Calculate the proportion of times the simulation produced values within the 95% confidence interval. Print this value to the console.
mean(res)



#Exercise 04
# The vector of filtered heights 'x' has already been loaded for you. Calculate the mean.
mu <- mean(x)

# Use the same sampling parameters as in the previous exercise.
set.seed(1)
N <- 15
B <- 10000

# Generate a logical vector 'res' that contains the results of the simulations using the t-distribution
res <- replicate(B, {
  X <- sample(x, N, replace = TRUE)
  se <- sd(X)/sqrt(N)
  interval<- c(mean(X) - qt(0.975, N-1) * se, mean(X) + qt(0.975, N-1) * se)
  between(mu, interval[1], interval[2])
})

# Calculate the proportion of times the simulation produced values within the 95% confidence interval. Print this value to the console.
mean(res)
