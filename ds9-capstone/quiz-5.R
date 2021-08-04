setwd("~/projects/data-science-course/ds9-capstone")
#edx2 <- read_csv(file = "./dat/edx2.csv")

sum(str_detect(edx2$genres, "Drama"))
gen <- c("Drama", "Comedy", "Thriller", "Romance")

howmany <- function(x){
  sum(str_detect(edx2$genres, x))
}

howmany("Drama")
sapply(gen, howmany)
