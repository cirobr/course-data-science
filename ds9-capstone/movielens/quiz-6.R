setwd("~/projects/data-science-course/ds9-capstone")
#edx <- read_csv(file = "./dat/edx.csv")

edx %>% group_by(movieId, title) %>%
  summarize(count = n()) %>%
  arrange(desc(count))