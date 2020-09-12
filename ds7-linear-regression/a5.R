library(Lahman)
library(tidyverse)
library(dslabs)
ds_theme_set()

tt <- Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(R_per_game = R / G,
         HR_per_game = HR / G,
         BB_per_game = BB / G)

tt %>% lm(R_per_game ~ BB_per_game + HR_per_game, data = .)


B <- 1000
N <- 100
lse <- replicate(B, {
  sample_n(galton_heights, N, replace = TRUE) %>% 
    lm(son ~ father, data = .) %>% .$coef 
})

lse <- data.frame(beta_0 = lse[1,], beta_1 = lse[2,])
