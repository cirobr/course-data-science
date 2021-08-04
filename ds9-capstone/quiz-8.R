#edx <- read_csv(file = "./dat/edx.csv")

q <- edx %>%
  group_by(rating) %>%
  summarize(qty = n()) %>%
  arrange(desc(qty))
q

q %>% 
  mutate(grp = ifelse((rating%%1 == 0), "whole", "half")) %>%
  group_by(grp) %>%
  summarize(q_grp = sum(qty))
