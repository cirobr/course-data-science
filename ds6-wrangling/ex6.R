tab1 <- data.frame("state" = c("Alabama", "Alaska", "Arizona", "Delaware", "District of Columbia"),
                   "population" = c(4779736, 710231, 6392017, 897934, 601723)
)

tab2 <- data.frame("state" = c("Alabama", "Alaska", "Arizona", "California", "Colorado", "Connecticut"),
                  "electoral_votes" = c(9, 3, 11, 55, 9, 7)
)

dat <- left_join(tab1, tab2, by = "state")
dat

right_join(tab1, tab2, by = "state")
full_join(tab1, tab2, by = "state")
inner_join(tab1, tab2, by = "state")
semi_join(tab1, tab2, by = "state")