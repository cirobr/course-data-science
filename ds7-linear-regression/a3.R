library(dplyr)
library(ggplot2)
options(digits = 3)

#set.seed(1989) #if you are using R 3.5 or earlier
set.seed(1989, sample.kind="Rounding") #if you are using R 3.6 or later
library(HistData)
data("GaltonFamilies")

female_heights <- GaltonFamilies%>%     
  filter(gender == "female") %>%     
  group_by(family) %>%     
  sample_n(1) %>%     
  ungroup() %>%     
  select(mother, childHeight) %>%     
  rename(daughter = childHeight)

female_heights %>%
  summarize(mean(mother), sd(mother), mean(daughter), sd(daughter))

female_heights %>% summarize(r = cor(mother, daughter)) %>% pull(r)

# calculate values to plot regression line on original data
mu_x <- mean(female_heights$mother)
mu_y <- mean(female_heights$daughter)
s_x <- sd(female_heights$mother)
s_y <- sd(female_heights$daughter)
r <- cor(female_heights$mother, female_heights$daughter)
m <- r * s_y/s_x
b <- mu_y - m*mu_x
c(m, b)

r^2 * 100

X <- 60
mu_y + r * (X - mu_x)/s_x * s_y