dat <- data.frame("state" = c("Alabama", "Alabama", "Alaska", "Alaska", "Arizona", "Arizona"),
                "abb" = c("AL", "AL", "AK", "AK", "AZ", "AZ"),
                "region" = c("South", "South", "West", "West", "West", "West"),
                "var" = c("population", "total", "population", "total", "population", "total"),
                "people" = c(4779736, 135, 710231, 19, 6392017, 232))
dat

dat %>% spread(key = var, value = people)