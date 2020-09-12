library(rvest)
library(tidyverse)
library(stringr)
url <- "https://en.wikipedia.org/w/index.php?title=Opinion_polling_for_the_United_Kingdom_European_Union_membership_referendum&oldid=896735054"
tab <- read_html(url) %>% html_nodes("table")
polls <- tab[[5]] %>% html_table(fill = TRUE)
head(polls)

colunas <- c("dates", "remain", "leave", "undecided", "lead", "samplesize", "pollster", "poll_type", "notes")
colnames(polls) <- colunas
head(polls)
nrow(polls)

polls <- polls %>% filter(str_detect(remain, "%"))
nrow(polls)

polls$remain <- parse_number(polls$remain)/100
#as.numeric(str_replace(polls$remain, "%", ""))/100
polls$remain

polls$undecided <- str_replace(polls$undecided, "N/A", "0")
polls$undecided

y <- "\\d+\\s[a-zA-Z]{3,5}"
temp <- str_extract_all(polls$dates, y)
end_date <- sapply(temp, function(x) x[length(x)]) # take last element (handles polls that cross month boundaries)
head(polls$dates)
head(end_date)

