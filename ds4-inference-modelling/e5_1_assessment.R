# Load the libraries and poll data
library(dplyr)
library(dslabs)
data(polls_us_election_2016)

# Create an object `polls` that contains the spread of predictions for each candidate in Florida during the last polling days
polls <- polls_us_election_2016 %>% 
  filter(state == "Florida" & enddate >= "2016-11-04" ) %>% 
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100)

# Examine the `polls` object using the `head` function
head(polls)

# Create an object called `results` that has two columns containing the average spread (`avg`) and the standard error (`se`). Print the results to the console.
results <- polls %>% summarize(avg = mean(spread), se = sd(spread) / sqrt(length(spread)))
results

# Define `mu` and `tau`
mu <- 0
tau <- 0.01

# Define a variable called `sigma` that contains the standard error in the object `results`
sigma <- results$se
  
# Define a variable called `Y` that contains the average in the object `results`
Y <- results$avg

# Define a variable `B` using `sigma` and `tau`. Print this value to the console.
B <- sigma ^ 2 / (sigma^2 + tau^2)
B

# Calculate the expected value of the posterior distribution
B * mu + (1 - B) * Y

# Compute the standard error of the posterior distribution. Print this value to the console.
se <- sqrt(1 / (1/sigma^2 + 1/tau^2))
se

# Construct the 95% credible interval. Save the lower and then the upper confidence interval to a variable called `ci`.
Ep_y <- B * mu + (1-B) * Y
exp_value <- Ep_y

ci <- c(Ep_y - qnorm(0.975) * se, Ep_y + qnorm(0.975) * se)
ci

# Using the `pnorm` function, calculate the probability that the actual spread was less than 0 (in Trump's favor). Print this value to the console.
pnorm(0, exp_value, se)

# Define a variable `taus` as different values of tau
taus <- seq(0.005, 0.05, len = 100)

# Create a function called `p_calc` that generates `B` and calculates the probability of the spread being less than 0
p_calc <- function(x){
  B <- sigma ^ 2 / (sigma^2 + x^2)
  se <- sqrt(1 / (1/sigma^2 + 1/x^2))
  exp_value <- B * mu + (1-B) * Y
  pnorm(0, exp_value, se)
}

# Create a vector called `ps` by applying the function `p_calc` across values in `taus`
ps <- p_calc(taus)

# Plot `taus` on the x-axis and `ps` on the y-axis
plot(taus, ps)
