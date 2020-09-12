#set.seed(1989) #if you are using R 3.5 or earlier
set.seed(1989, sample.kind="Rounding") #if you are using R 3.6 or later
library(HistData)
data("GaltonFamilies")
options(digits = 3)    # report 3 significant digits

female_heights <- GaltonFamilies %>%     
  filter(gender == "female") %>%     
  group_by(family) %>%     
  sample_n(1) %>%     
  ungroup() %>%     
  select(mother, childHeight) %>%     
  rename(daughter = childHeight)

# fit regression line to predict son's height from father's height
fit <- lm(mother ~ daughter, data = female_heights)
fit



m <- fit %>% .$coef %>% .[2]
m <- as.numeric(m)
b <- fit %>% .$coef %>% .[1]
b <- as.numeric(b)
Y_hat <- b + m * female_heights$daughter[1]
c(Y_hat, female_heights$mother[1])

