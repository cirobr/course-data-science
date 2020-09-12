library(rvest)
library(tidyverse)
url <- "https://en.wikipedia.org/w/index.php?title=Opinion_polling_for_the_United_Kingdom_European_Union_membership_referendum&oldid=896735054"

h <- read_html(url)
tab <- html_nodes(h, "table")
tab

for(i in 1:length(tab)){
  x <- html_table(tab[[i]], fill = TRUE)
  if(colnames(x) == "Date(s) conducted" & ncol(x) == 9){
    print(i)
    print(colnames(x))
  }
}