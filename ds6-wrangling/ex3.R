stats <- data.frame("key" = c("allen_height", "allen_hand_length", "allen_wingspan", "bamba_height", "bamba_hand_length", "bamba_wingspan"),
                    "value" = c(75, 8.25, 79.25, 83.25, 9.75, 94))
stats

tidy_data1 <- stats %>%
  separate(col = key, into = c("player", "variable_name"), sep = "_", extra = "merge") %>% 
  spread(key = variable_name, value = value)
tidy_data1

tidy_data2 <- stats %>%
  separate(col = key, into = c("player", "variable_name1", "variable_name2"), sep = "_", fill = "right") %>% 
  unite(col = variable_name, variable_name1, variable_name2, sep = "_") %>% 
  spread(key = variable_name, value = value)
tidy_data2

tidy_data3 <- stats %>%
  separate(col = key, into = c("player", "variable_name"), sep = "_") %>% 
  spread(key = variable_name, value = value)
tidy_data3
