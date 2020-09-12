# Exercicio 01

library(dslabs)
library(tidyverse)
data("polls_us_election_2016")

polls <- polls_us_election_2016 %>% filter(enddate >= "2016-10-31" & state == "U.S.")

nrow(polls)

N <- polls$samplesize[1]
N

X_hat <- polls$rawpoll_clinton[1]/100
X_hat

se_hat <- sqrt(X_hat * (1 - X_hat) / N)
se_hat

ucl <- 0.95
ucl_tail <- (1 - ucl)/2

z_low <- qnorm(ucl_tail, X_hat, se_hat)
z_upp <- qnorm(ucl_tail + ucl, X_hat, se_hat)
ci <- c(z_low, z_upp)

ci
pnorm(z_upp, X_hat, se_hat) - pnorm(z_low, X_hat, se_hat)   # deve ser igual a ucl


#Exercicio 02
polls <- mutate(polls, X_hat = rawpoll_clinton/100, se_hat = sqrt(X_hat * (1 - X_hat) / samplesize), lower = qnorm(ucl_tail, X_hat, se_hat), upper = qnorm(ucl + ucl_tail, X_hat, se_hat))

pollster_results <- select(polls, pollster, enddate, X_hat, se_hat, lower, upper)


#Exercicio 03
p <- 0.482
pollster_results <- mutate(pollster_results, hit = (p >= lower & p <= upper))
avg_hit <- pollster_results %>% summarize(mean(hit))
avg_hit
mean(pollster_results$hit)
