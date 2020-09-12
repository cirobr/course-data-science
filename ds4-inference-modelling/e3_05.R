# Exercicio 05

library(dslabs)
library(tidyverse)
data("polls_us_election_2016")

polls <- polls_us_election_2016 %>% filter(enddate >= "2016-10-31" & state == "U.S.") 
polls <- mutate(polls, d_hat = rawpoll_clinton - rawpoll_trump)

N <- polls$samplesize[1]
N

d_hat <- polls$d_hat[1]/100
d_hat

X_hat <- (d_hat+1)/2

se_hat <- sqrt(X_hat*(1 - X_hat) / N)
se_spread <- 2 * se_hat
se_hat <- se_spread

ucl <- 0.95
ucl_tail <- (1 - ucl)/2
ci <- c(d_hat - qnorm(1 - ucl_tail)*se_hat, d_hat + qnorm(1 - ucl_tail)*se_hat)
