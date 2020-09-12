library(tidyverse)
library(dslabs)

data(admissions)
dat <- admissions %>% select(-applicants)

spread(dat, gender, admitted)

tmp <- gather(admissions, key, value, admitted:applicants)
tmp

tmp2 <- unite(tmp, column_name, c(key, gender))
tmp2
