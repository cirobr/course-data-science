library(tidyverse)
library(broom)
library(Lahman)
Teams_small <- Teams %>% 
  filter(yearID %in% 1961:2001) %>% 
  mutate(avg_attendance = attendance/G)

library(dslabs)
ds_theme_set()
options(digits = 3)

Teams_small %>%
  mutate(R_per_game = R / G) %>%
  lm(avg_attendance ~ R_per_game, data = .) %>%
  tidy(conf.int = TRUE)

Teams_small %>%
  mutate(HR_per_game = HR / G) %>%
  lm(avg_attendance ~ HR_per_game, data = .) %>%
  tidy(conf.int = TRUE)


fit <- Teams_small %>%
  lm(avg_attendance ~ W, data = .) %>%
  tidy(conf.int = TRUE)
fit

x <- 0
m <- fit$estimate[2]
b <- fit$estimate[1]
Y_hat <- m * x + b
Y_hat

Teams_small %>%
  group_by(yearID) %>%
  #summarise(avg = sum(attendance) / sum(G)) %>%
  lm(avg_attendance ~ yearID, data = .) %>%
  tidy()



Teams_small %>%
  mutate(R_per_game = R / G) %>%
  summarize(r = cor(W, R_per_game)) %>%
  pull(r)

Teams_small %>%
  mutate(HR_per_game = HR / G) %>%
  summarize(r = cor(W, HR_per_game)) %>%
  pull(r)



Teams_strata <- Teams_small %>%
  mutate(strata_W = round(W/10)) %>%
  group_by(strata_W) %>%
  filter(strata_W %in% 5:10 & n() >= 20)
Teams_strata

Teams_strata %>%
  filter(strata_W == 8) %>%
  nrow()

dat1 <- Teams_strata %>%
  mutate(R_per_game = R / G) %>%
  do(tidy(lm(avg_attendance ~ R_per_game, data = .), conf.int = TRUE)) %>%
  filter(term == "R_per_game") %>%
  select(strata_W, estimate)
dat1
ind <- which.max(dat1$estimate)
dat1[ind,]

dat2 <- Teams_strata %>%
  mutate(HR_per_game = HR / G) %>%
  do(tidy(lm(avg_attendance ~ HR_per_game, data = .), conf.int = TRUE)) %>%
  filter(term == "HR_per_game") %>%
  select(strata_W, estimate)
dat2
ind <- which.max(dat1$estimate)
dat2[ind,]



Teams_strata %>%
  mutate(HR_per_game = HR / G) %>%
  summarize(r = cor(avg_attendance, HR_per_game)) %>%
  ggplot(aes(strata_W, r)) +
  geom_point() +
  geom_line() +
  ggtitle("Correlação HR / G")


dat3 <- Teams_small %>%
  mutate(R_per_game = R / G,
         HR_per_game = HR / G) %>%
  lm(avg_attendance ~ R_per_game + HR_per_game + W + yearID, data = .) %>%
  tidy() %>%
  select(term, estimate)

rpg_x <- 5
hrpg_x <- 1.2
w_x <- 80
year_x <- c(2002, 1960)

int_beta <- dat3$estimate[1]
rpg_beta <- dat3$estimate[2]
hrpg_beta <- dat3$estimate[3]
w_beta <- dat3$estimate[4]
year_beta <- dat3$estimate[5]

Y_hat <- int_beta + rpg_beta * rpg_x + hrpg_beta * hrpg_x + w_beta * w_x + year_beta * year_x
Y_hat



year_x <- 2002

Teams %>%
  filter(yearID == year_x) %>%
  mutate(R_per_game = R / G,
         HR_per_game = HR / G,
         avg_attendance = attendance/G) %>%
  select(yearID, G, R_per_game, HR_per_game, W, avg_attendance, attendance) %>%
  mutate(Y_hat = int_beta + rpg_beta * R_per_game + hrpg_beta * HR_per_game + w_beta * W + year_beta * yearID) %>%
  mutate(Y_hat = Y_hat * G) %>%
  summarize(r = cor(Y_hat, attendance)) %>% pull(r)
