library(Lahman)
library(tidyverse)
library(dslabs)
library(broom)
ds_theme_set()
options(digits = 3)

# linear regression with two variables
fit <- Teams %>% 
  filter(yearID %in% 1971) %>% 
  mutate(BB = BB/G,
         HR = HR/G,
         R = R/G) %>%
  lm(R ~ BB + HR, data = .)
coefs <- tidy(fit, conf.int = TRUE)
coefs

bb_beta <- coefs$estimate[2]
bb_beta
hr_beta <- coefs$estimate[3]
hr_beta



coefs %>% filter(p.value < 0.05)



# not useful group_by linear regression
Teams %>%
  filter(yearID %in% 1961:2018) %>%
  group_by(yearID) %>%
  mutate(BB = BB/G,
         HR = HR/G,
         R = R/G) %>%
  do(fit = lm(R ~ BB + HR, data = .))

# rewriting group_by linear regression with tydy and do function
dat <- Teams %>%
  filter(yearID %in% 1961:2018) %>%
  group_by(yearID) %>%
  mutate(BB = BB/G,
         HR = HR/G,
         R = R/G) %>%
  do(tidy(lm(R ~ BB + HR, data = .), conf.int = TRUE))
dat

p <- dat %>%
  filter(term == "BB") %>%
  ggplot(aes(yearID, estimate)) +
  geom_point() +
  geom_line() +
  geom_smooth(method = "lm")
p


### Famosa questão 11 ###
Teams %>%
  filter(yearID %in% 1961:2018) %>%
  group_by(yearID) %>%
    do(tidy(lm(R ~ BB + HR, data = .))) %>%
  filter(term == "BB") %>%
  lm(estimate ~ yearID, data = .) %>%
  tidy()
