library(tidyverse)
library(Lahman)
library(broom)

dat <- Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(HR = HR/G,
         R = R/G) %>%
  select(lgID, HR, BB, R)

#1
dat %>% 
  group_by(lgID) %>% 
  do(tidy(lm(R ~ HR, data = .), conf.int = T)) %>% 
  filter(term == "HR") 

#2
dat %>% 
  group_by(lgID) %>% 
  do(glance(lm(R ~ HR, data = .)))

#3
dat %>% 
  do(tidy(lm(R ~ HR, data = .), conf.int = T)) %>% 
  filter(term == "HR")

#4
dat %>% 
  group_by(lgID) %>% 
  do(mod = lm(R ~ HR, data = .))
