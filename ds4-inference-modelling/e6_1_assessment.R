# Load the libraries and data
library(dplyr)
library(dslabs)
data("polls_us_election_2016")

# Create a table called `polls` that filters by  state, date, and reports the spread
polls <- polls_us_election_2016 %>% 
  filter(state != "U.S." & enddate >= "2016-10-31") %>% 
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100)

head(polls)

# Create an object called `cis` that has the columns indicated in the instructions
cis <- polls %>% mutate(X_hat = (spread+1)/2,
                        se = 2*sqrt(X_hat*(1-X_hat)/samplesize), 
                        lower = spread - qnorm(0.975)*se,
                        upper = spread + qnorm(0.975)*se) %>%
  select(state, startdate, enddate, pollster, grade, spread, lower, upper)

# Add the actual results to the `cis` data set
add <- results_us_election_2016 %>%
  mutate(actual_spread = clinton/100 - trump/100) %>%
  select(state, actual_spread)

ci_data <- cis %>%
  mutate(state = as.character(state)) %>%
  left_join(add, by = "state")

hit <- ci_data %>%
  mutate(hit = (actual_spread >= lower & actual_spread <= upper))

p_hits <- hit %>% summarise(mean(hit))
p_hits

p_hits <- hit %>%
  group_by(pollster) %>%
  filter(n() >= 5) %>%
  summarize(grade = grade[1], n = n(), proportion_hits = mean(hit)) %>%
  arrange(desc(proportion_hits))

# The `cis` data have already been loaded for you
add <- results_us_election_2016 %>%
  mutate(actual_spread = clinton/100 - trump/100) %>%
  select(state, actual_spread)

ci_data <- cis %>%
  mutate(state = as.character(state)) %>%
  left_join(add, by = "state")

hit <- ci_data %>%
  mutate(hit = (actual_spread >= lower & actual_spread <= upper))

p_hits <- hit %>%
  group_by(state) %>%
  filter(n() >= 5) %>%
  summarize(n = n(), proportion_hits = mean(hit)) %>%
  arrange(desc(proportion_hits))

# The `p_hits` data have already been loaded for you. Use the `head` function to examine it.
head(p_hits)

# Make a barplot of the proportion of hits for each state
p_hits <- p_hits %>%
  arrange(proportion_hits)

library(ggplot2)
p_hits %>%
  ggplot(aes(state, proportion_hits)) +
  geom_bar(stat = "identity") +
  coord_flip()

# The `cis` data have already been loaded. Examine it using the `head` function.
head(cis)

# Create an object called `errors` that calculates the difference between the predicted and actual spread and indicates if the correct winner was predicted
cis <- mutate(errot = )




