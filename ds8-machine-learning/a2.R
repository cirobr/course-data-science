library(dslabs)
library(dplyr)
library(lubridate)
data(reported_heights)

dat <- mutate(reported_heights, date_time = ymd_hms(time_stamp)) %>%
  filter(date_time >= make_date(2016, 01, 25) & date_time < make_date(2016, 02, 1)) %>%
  mutate(type = ifelse(day(date_time) == 25 & hour(date_time) == 8 & between(minute(date_time), 15, 30), "inclass","online")) %>%
  select(sex, type)

y <- factor(dat$sex, c("Female", "Male"))
x <- dat$type


#Q1
options(digits = 2)
tab <- dat %>% filter(type == "inclass") %>% table() %>% prop.table()
tab
prev_inclass <- "Female"

tab <- dat %>% filter(type == "online") %>% table() %>% prop.table()
tab
prev_online <- "Male"


#Q2
# generate training and test sets
set.seed(2, sample.kind = "Rounding") # if using R 3.5 or earlier, remove the sample.kind argument
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
test_set <- dat[test_index, ]
train_set <- dat[-test_index, ]

# guess the outcome
y_hat <- ifelse(x == "inclass", prev_inclass, prev_online) %>% factor()
mean(y == y_hat)


#Q3
table(y_hat, y)
confusionMatrix(data = y_hat, reference = y)


#Q4
sensitivity(y_hat, y)


#Q5
specificity(y_hat, y)


#Q6
dat$sex <- y
mean(dat$sex == "Female")
