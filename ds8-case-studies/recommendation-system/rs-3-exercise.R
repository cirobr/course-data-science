library(tidyverse)
library(lubridate)
library(dslabs)
data("movielens")


# q1
head(movielens)
p <- movielens %>%
  group_by(movieId, year) %>%
  summarize(nr = n())

p %>%
  ggplot(aes(year, nr, group=movieId)) +
  geom_boxplot() +
  scale_y_continuous(trans = "sqrt")

q <- p %>%
  ungroup() %>%
  group_by(year) %>%
  summarize(md = median(nr))
head(q)
max(q$md)
q[which.max(q$md),]


# q2
p %>%
  ungroup() %>%
  filter(year >= 1993) %>%
  group_by(movieId) %>%
  summarize(mean(nr))
