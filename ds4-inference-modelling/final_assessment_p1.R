### Brexit poll analysis p.1
#install.packages("dslabs")
options(digits = 3)

# suggested libraries and options
library(tidyverse)

# load brexit_polls object
library(dslabs)
data(brexit_polls)

p <- 0.481    # official proportion voting "Remain"
d <- 2*p-1    # official spread

N <- 1500

set.seed(1)
Remain <- sample(c(0,1), size = N, replace = TRUE, prob = c(1-p, p))    # generate N observations

# What is the expected total number of voters in the sample choosing "Remain"?
N * (1 * p + 0 * (1-p))

# What is the standard error of the total number of voters in the sample choosing "Remain"?
sqrt(N) * 1 * sqrt(p * (1-p))

# What is the expected value of X^, the proportion of "Remain" voters?
p

# What is the standard error of X^, the proportion of "Remain" voters?
X_hat <- mean(Remain)
se_hat <- sqrt(X_hat*(1-X_hat)/N)
se_hat

# What is the expected value of d, the spread between the proportion of "Remain" voters and "Leave" voters?
d

# What is the standard error of d, the spread between the proportion of "Remain" voters and "Leave" voters?
2 * se_hat



data("brexit_polls")
brexit_polls <- brexit_polls %>%
  mutate(x_hat = (spread + 1)/2)
head(brexit_polls)

# What is the average of the observed spreads (spread)?
# What is the standard deviation of the observed spreads?
# What is the average of x_hat, the estimates of the parameter p?
# What is the standard deviation of x_hat?
vec <- brexit_polls %>% summarize(mean_s = mean(spread), sd_s = sd(spread), mean_x = mean(x_hat), sd_x = sd(x_hat))
vec

first_poll <- brexit_polls[1,]
first_poll

ci_perc <- 0.95
tail <- (1 - ci_perc)/2

#e1 <- first_poll$spread
e1 <- first_poll$x_hat
n1 <- first_poll$samplesize
se1 <- sqrt(e1 * (1 - e1) / n1)

ci <- c(e1 - qnorm(ci_perc + tail) * se1, e1 + qnorm(ci_perc + tail) * se1)
ci
between(p, ci[1], ci[2])
