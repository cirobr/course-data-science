#edx <- read_csv(file = "./dat/edx.csv")

q <- edx %>%
  group_by(rating) %>%
  summarize(qty = n()) %>%
  arrange(desc(qty))
q
