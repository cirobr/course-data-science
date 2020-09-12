#install.packages("Lahman")
library(Lahman)
library(dplyr)
top <- Batting %>% 
  filter(yearID == 2016) %>%
  arrange(desc(HR)) %>%    # arrange by descending HR count
  slice(1:10)    # take entries 1-10
top %>% as_tibble()

Master %>% as_tibble()

top_names <- top %>% left_join(Master) %>%
  select(playerID, nameFirst, nameLast, HR)
top_names

top_salary <- Salaries %>% filter(yearID == 2016) %>%
  right_join(top_names) %>%
  select(nameFirst, nameLast, teamID, HR, salary)
top_salary

head(AwardsPlayers)
tab <- AwardsPlayers %>% filter(yearID == 2016)
head(tab)

inner_join(top, tab, "playerID") %>% select(playerID) %>% group_by(playerID)

anti_join(tab, top, "playerID") %>% select(playerID) %>% group_by(playerID)
