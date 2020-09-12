library(Lahman)
library(tidyverse)
library(dslabs)
ds_theme_set()

options(digits = 3)

teams_table <- Teams %>% filter(yearID %in% 1961:2001)

teams_table %>%
  mutate(AB_per_game = AB/G, R_per_game = R/G) %>%
  ggplot(aes(AB_per_game, R_per_game)) + 
  geom_point(alpha = 0.5)

teams_table %>%
  mutate(AB_per_game = AB/G, R_per_game = R/G) %>%
  select(AB_per_game, R_per_game) %>%
  summarize(r = cor(AB_per_game, R_per_game)) %>% pull(r)


teams_table %>%
  mutate(win_rate = W/G, E_per_game = E/G) %>%
  ggplot(aes(win_rate, E_per_game)) + 
  geom_point(alpha = 0.5)

teams_table %>%
  mutate(win_rate = W/G, E_per_game = E/G) %>%
  select(win_rate, E_per_game) %>%
  summarize(r = cor(win_rate, E_per_game)) %>% pull(r)


teams_table %>%
  mutate(triples_per_game = X3B/G, doubles_per_game = X2B/G) %>%
  ggplot(aes(triples_per_game, doubles_per_game)) + 
  geom_point(alpha = 0.5)

teams_table %>%
  mutate(triples_per_game = X3B/G, doubles_per_game = X2B/G) %>%
  select(triples_per_game, doubles_per_game) %>%
  summarize(r = cor(triples_per_game, doubles_per_game)) %>% pull(r)
