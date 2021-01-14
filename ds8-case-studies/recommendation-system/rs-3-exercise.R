library(tidyverse)
library(lubridate)
library(dslabs)
data("movielens")
options(digits = 3)


# q1
head(movielens)
p <- movielens %>%
  group_by(movieId, year) %>%
  summarize(n_ratings = n())
head(p)


#p %>%
#  ggplot(aes(year, n_ratings, group=movieId)) +
#  geom_boxplot() +
#  scale_y_continuous(trans = "sqrt")


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
  filter(movieId == 318,
  year >= 1993) %>%
  summarize(n = n(),
            s = sum(rating))
tab2
tab2$s / tab2$n


# q3
tab3 <- movielens %>%
  filter(year >= 1993) %>%
  mutate(years = 2018 - year) %>%
  group_by(movieId) %>%
  select(movieId, year, userId, rating, years) %>%
  summarize(avg_rating_per_yr=mean(n()/years)) %>%
  distinct() %>%
  arrange(desc(avg_rating_per_yr))
  
tab4 <- left_join(tab3, p)
plot(tab4$avg_rating_per_yr, tab4$n_ratings)


# q5
movielens <- mutate(movielens, 
                    week = round_date(as_datetime(timestamp),
                                                 unit = "week"))
# q6
movielens %>%
  group_by(week) %>%
  summarise(avg_rating = mean(rating)) %>%
  ggplot(aes(week, avg_rating)) +
  geom_smooth() + geom_point()


# q8
movielens %>%
  group_by(genres) %>%
  summarize(n = n(), av = mean(rating), se = sd(rating)/sqrt(n())) %>%
  filter(n >= 1000) %>%
  ggplot(aes(x = genres, y = av, ymin = av - 2*se, ymax = av + 2*se)) +
  geom_point() + geom_errorbar()
