library(tidyverse)
s <- c("5'10", "6'1\"", "5'8inches", "5'7.5")
s
tab <- data.frame(x = s)
tab

tab %>% extract(x, c("feet", "inches"), regex = "(\\d)'(\\d{1,2})")

extract(data = tab, col = x, into = c("feet", "inches", "decimal"),
        regex = "(\\d)'(\\d{1,2})(\\.)?")

extract(data = tab, col = x, into = c("feet", "inches", "decimal"),
        regex = "(\\d)'(\\d{1,2})(\\.\\d+)")

extract(data = tab, col = x, into = c("feet", "inches", "decimal"),
        regex = "(\\d)'(\\d{1,2})(\\.\\d+)?")


extract(data = tab, col = x, into = c("feet", "inches", "decimal"),
        regex = "(\\d)'(\\d{1,2})\\.\\d+?")
