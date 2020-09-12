# Exercicio 06

library(dslabs)
library(tidyverse)
data("polls_us_election_2016")

ucl <- 0.95
ucl_tail <- (1 - ucl)/2

polls <- polls_us_election_2016 %>% filter(enddate >= "2016-10-31" & state == "U.S.") 
polls <- mutate(polls,
                d_hat = rawpoll_clinton - rawpoll_trump,
                X_hat = (d_hat + 1) / 2,
                se_hat = 2 * sqrt(X_hat*(1 - X_hat) / samplesize),
                lower = d_hat - qnorm(1 - ucl_tail)*se_hat,
                upper = d_hat + qnorm(1 - ucl_tail)*se_hat
                )

pollster_results <- polls %>% select(pollster, enddate, d_hat, lower, upper)
head(pollster_results)

#Exercicio 07

d <- 0.021
polls <- mutate(polls,
                error = d_hat - d)

polls %>% group_by(pollster) %>%
  filter(n() >= 5) %>%
  ggplot(aes(x = pollster, y = error)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))