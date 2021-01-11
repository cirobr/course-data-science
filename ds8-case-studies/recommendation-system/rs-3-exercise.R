library(tidyverse)
library(lubridate)
library(dslabs)
data("movielens")


# q1
head(movielens)
p <- movielens %>%
  group_by(year) %>%
  summarize(nr = n())
