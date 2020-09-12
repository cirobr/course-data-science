library(dplyr)
library(tidyverse)
library(Lahman)
library(dslabs)
options(digits = 3)
ds_theme_set()


bat_02 <- Batting %>% filter(yearID == 2002) %>%
  mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
  filter(pa >= 100) %>%
  select(playerID, singles, bb)

bat_new <- Batting %>%
  filter(yearID %in% 1999:2001) %>%
  mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
  filter(pa >= 100) %>%
  select(playerID, singles, bb) %>%
  group_by(playerID) %>%
  summarise(mean_singles = mean(singles), mean_bb = mean(bb))

bat_new %>% filter(mean_singles > 0.2) %>% nrow()
bat_new %>% filter(mean_bb > 0.2) %>% nrow()

tab <- inner_join(bat_02, bat_new, by = "playerID")
tab %>% summarize(r = cor(singles, mean_singles))
tab %>% summarize(r = cor(bb, mean_bb))

tab %>%
  ggplot(aes(mean_singles, singles)) +
  geom_point(alpha = 0.5)

tab %>%
  ggplot(aes(mean_bb, bb)) +
  geom_point(alpha = 0.5)

lm(singles ~ mean_singles, data = tab)
lm(bb ~ mean_bb, data = tab)
