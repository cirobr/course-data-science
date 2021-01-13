library(tidyverse)
library(lubridate)
library(dslabs)
data("movielens")


# q1
head(movielens)
p <- movielens %>%
  group_by(movieId, year) %>%
  summarize(n_ratings = n())
head(p)

p %>%
  ggplot(aes(year, n_ratings, group=movieId)) +
  geom_boxplot() +
  scale_y_continuous(trans = "sqrt")

q <- p %>%
  ungroup() %>%
  group_by(year) %>%
  summarize(md = median(n_ratings))
head(q)
max(q$md)
q[which.max(q$md),]


# q2
movies <- movielens %>%
  select(movieId, title) %>%
  distinct()

tab1 <- movielens %>%
  filter(year >= 1993) %>%
  mutate(qtd_yr=2018-year) %>%
  group_by(movieId) %>%
  summarize(avg_rating_per_yr=mean(n()/qtd_yr)) %>%
  arrange(desc(avg_rating_per_yr)) %>%
  top_n(n=25)

left_join(tab1, movies) %>%
  filter(title == "Forrest Gump")

tab2 <- movielens %>%
  filter(year >= 1993)

tab2 %>%
  filter(title == "Shawshank Redemption, The") %>%
  head()
