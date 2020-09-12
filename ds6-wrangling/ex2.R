d <- data.frame("age_group" = c(20, 30, 40, 50),
                  "2015_time" = c("3:46", "3:50", "4:39", "4:48"),
                  "2015_participants" = c(54, 60, 29, 10),
                  "2016_time" = c("3:22", "3:43", "3:49", "4:59"),
                  "2016_participants" = c(62, 58, 33, 14))
d

#tidy_data1 <- d %>%
  #gather(key = "key", value = "value", -age_group) %>%
  #separate(col = key, into = c("year", "variable_name"), sep = ".") %>% 
  #spread(key = variable_name, value = value)

tidy_data2 <- d %>%
  gather(key = "key", value = "value", -age_group) %>%
  separate(col = key, into = c("year", "variable_name"), sep = "_") %>% 
  spread(key = variable_name, value = value)

tidy_data3 <- d %>%
  gather(key = "key", value = "value") %>%
  separate(col = key, into = c("year", "variable_name"), sep = "_") %>% 
  spread(key = variable_name, value = value)

tidy_data4 <- d %>%
  gather(key = "key", value = "value", -age_group) %>%
  separate(col = key, into = "year", sep = "_") %>% 
  spread(key = year, value = value)
