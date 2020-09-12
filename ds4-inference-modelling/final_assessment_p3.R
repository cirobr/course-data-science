### Brexit poll analysis p.3
options(digits = 3)

# suggested libraries
library(tidyverse)

# load brexit_polls object and add x_hat column
library(dslabs)
data(brexit_polls)
brexit_polls <- brexit_polls %>%
  mutate(x_hat = (spread + 1)/2)

# final proportion voting "Remain"
p <- 0.481

brexit_hit <- brexit_polls %>%
  mutate(p_hat = (spread + 1)/2,
         se_spread = 2*sqrt(p_hat*(1-p_hat)/samplesize),
         spread_lower = spread - qnorm(.975)*se_spread,
         spread_upper = spread + qnorm(.975)*se_spread,
         hit = spread_lower < -0.038 & spread_upper > -0.038) %>%
  select(poll_type, hit)

br1 <- brexit_hit %>% group_by(poll_type, hit) %>% count()
br1

two_by_two <- tibble(hit = c("no", "yes"),
                     online = as.numeric(c(br1[1,3], br1[2,3])),
                     telephone = as.numeric(c(br1[3,3], br1[4,3])))
two_by_two

# chi-squared test
chisq_test <- two_by_two %>%
  select(-hit) %>%
  chisq.test()
chisq_test$p.value

# odds of getting online hits
odds_online <- (two_by_two$online[2] / sum(two_by_two$online)) /
  (two_by_two$online[1] / sum(two_by_two$online))
odds_online

#odds of getting telephone hits
odds_telephone <- (two_by_two$telephone[2] / sum(two_by_two$telephone)) /
  (two_by_two$telephone[1] / sum(two_by_two$telephone))
odds_telephone

# odds ratio - how many times larger odds are for online than telephone
odds_online / odds_telephone

# Use brexit_polls to make a plot of the spread (spread) over time (enddate) colored by poll type (poll_type).
# Use geom_smooth() with method = "loess" to plot smooth curves with a span of 0.4.
# Include the individual data points colored by poll type. Add a horizontal line indicating the final value of d=???.038.
head(brexit_polls)

brexit_polls %>%
  ggplot(aes(enddate, spread, col = poll_type)) +
  geom_smooth(method = "loess", span = 0.4) +
  geom_point() +
  geom_hline(aes(yintercept = -.038))



brexit_long <- brexit_polls %>%
  gather(vote, proportion, "remain":"undecided") %>%
  mutate(vote = factor(vote))

brexit_long %>%
  ggplot(aes(enddate, proportion, col = vote)) +
  geom_smooth(method = "loess", span = 0.3)





  