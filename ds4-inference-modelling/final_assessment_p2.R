### Brexit poll analysis p.2
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

june_polls <- brexit_polls %>% filter(enddate >= "2016-06-01") %>%
  mutate(se_x_hat = sqrt(x_hat * (1-x_hat) / samplesize),
         se_spread = 2 * se_x_hat,
         low =  (2 * x_hat - 1) - qnorm(ci_perc + tail) * se_spread,
         high = (2 * x_hat - 1) + qnorm(ci_perc + tail) * se_spread,
         hit = (d >= low & d <= high),
)

# How many polls are in june_polls?
june_polls
jun_total <- nrow(june_polls)
jun_total

# What proportion of polls have a confidence interval that covers the value 0?
jun_0 <- june_polls %>% filter((low <= 0 & high >= 0)) %>% nrow()
jun_0 / jun_total

# What proportion of polls predict "Remain" (confidence interval entirely above 0)?
jun_above <- june_polls %>% filter((low > 0)) %>% nrow()
jun_above / jun_total

# What proportion of polls have a confidence interval covering the true value of d?
jun_d <- june_polls %>% filter(hit == TRUE) %>% nrow()
jun_d / jun_total

# Group and summarize the june_polls object by pollster to find the proportion of hits for each pollster and the number of polls per pollster.
# Use arrange() to sort by hit rate.
june_polls %>% group_by(pollster) %>% summarize(hits = mean(hit), nr = n()) %>% arrange(desc(hits))

# Make a boxplot of the spread in june_polls by poll type.
june_polls %>% ggplot(aes(poll_type, spread)) + geom_boxplot()

# Calculate the confidence intervals of the spread combined across all polls in june_polls, grouping by poll type. 
# Recall that to determine the standard error of the spread, you will need to double the standard error of the estimate.

combined_by_type <- june_polls %>%
  group_by(poll_type) %>%
  summarize(N = sum(samplesize),
            spread = sum(spread*samplesize)/N,
            p_hat = (spread + 1)/2)

combined_by_type

# What is the lower bound of the 95% confidence interval for online voters?
# What is the upper bound of the 95% confidence interval for online voters?
combined_by_type %>% mutate(se_p_hat = sqrt(p_hat * (1-p_hat) / N),
                            se_spread = 2 * se_p_hat,
                            low =  (2 * p_hat - 1) - qnorm(ci_perc + tail) * se_spread,
                            high = (2 * p_hat - 1) + qnorm(ci_perc + tail) * se_spread,
                            high - low
)
