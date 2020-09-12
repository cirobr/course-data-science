library(tidyverse)
x <- data.frame("age_group" = c(20, 30, 40, 50, 20),
                "year" = c(2015, 2015, 2015, 2015, 2016),
                "time" = c("03:46", "03:50", "04:39", "04:48", "03:22"))
x

x %>% spread(year, time)