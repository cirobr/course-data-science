setwd("~/projects/data-science-course/ds9-capstone")
#edx <- read_csv(file = "./dat/edx.csv")

sum(str_detect(edx$genres, "Drama"))
gen <- c("Drama", "Comedy", "Thriller", "Romance")

howmany <- function(x){
  sum(str_detect(edx$genres, x))
}

howmany("Drama")
sapply(gen, howmany)
