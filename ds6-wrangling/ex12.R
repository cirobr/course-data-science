library(dslabs)
library(tidyverse)
library(lubridate)
data(brexit_polls)

sum(month(brexit_polls$startdate) == "4")
sum(round_date(brexit_polls$enddate, unit = "week") == "2016-06-12")

sort(table(weekdays(brexit_polls$enddate)), decreasing = TRUE)
