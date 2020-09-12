options(digits = 3)

library(dslabs)
library(dplyr)
data("research_funding_rates")
research_funding_rates

tab <- research_funding_rates %>%
  mutate(noawards_men = applications_men - awards_men,
         noawards_women = applications_women - awards_women) %>%
  select(applications_men, awards_men, noawards_men, applications_women, awards_women, noawards_women) %>%
  summarise(sum(awards_men), sum(noawards_men), sum(awards_women), sum(noawards_women))
  
tab <- as.numeric(tab)
tab <- matrix(tab, ncol = 2)
rownames(tab) <- c("award", "no_award")
colnames(tab) <- c("men", "women")
tab


tab2 <- research_funding_rates %>%
  select(applications_men, applications_women) %>%
  summarise(sum(applications_men), sum(applications_women))
tab2 <- as.numeric(tab2)
tab2 <- matrix(tab2, ncol = 2)
rownames(tab2) <- "applications"
colnames(tab2) <- c("men", "women")
tab["award",]/tab2 * 100

# chi-squared test
chisq_test <- tab %>%
  chisq.test()
chisq_test$p.value



dat <- research_funding_rates %>% 
  mutate(discipline = reorder(discipline, success_rates_total)) %>%
  rename(success_total = success_rates_total,
         success_men = success_rates_men,
         success_women = success_rates_women) %>%
  gather(key, value, -discipline) %>%
  separate(key, c("type", "gender")) %>%
  spread(type, value) %>%
  filter(gender != "total")
dat

dat %>%
  group_by(discipline) %>%
  mutate(overall_rank = sum(awards) / sum(applications)) %>%
  arrange(overall_rank) %>%
  ggplot(aes(discipline, success, col = gender, size = applications)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))