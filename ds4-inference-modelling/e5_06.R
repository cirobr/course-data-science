# Question 6

# Load the libraries and data you need for the following exercises
library(dslabs)
library(dplyr)
library(ggplot2)
data("polls_us_election_2016")

# These lines of code filter for the polls we want and calculate the spreads
polls <- polls_us_election_2016 %>% 
  filter(pollster %in% c("Rasmussen Reports/Pulse Opinion Research","The Times-Picayune/Lucid") &
           enddate >= "2016-10-15" &
           state == "U.S.") %>% 
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100) 

# Make a boxplot with points of the spread for each pollster
polls %>% ggplot(aes(pollster, spread)) +
  geom_boxplot() +
  geom_point()

# Question 13
sigma <- polls %>%
  group_by(pollster) %>%
  summarise(s = sd(spread))

# Question 15
res <- polls %>%
  group_by(pollster) %>%
  summarise(a = mean(spread), s = sd(spread), nr = n())

estimate <- max(res$a) - min(res$a)
estimate

se_hat <- sqrt(res$s[2]^2 / res$nr[2] + res$s[1]^2 / res$nr[1])
se_hat

ci <- c(estimate - qnorm(0.975)*se_hat, estimate + qnorm(0.975)*se_hat)

# Question 16
2 * (1 - pnorm(estimate / se_hat))

