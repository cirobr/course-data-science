setwd("~/projects/data-science-course/ds9-capstone")
#edx2 <- read_csv(file = "./dat/edx2.csv")

mov <- c("Forrest Gump", "Jurassic Park", "Pulp Fiction", "The Shawshank Redemption", "Speed 2: Cruise Control")
sum(is.na(edx2$rating))

howmany <- function(x){
  sum(str_detect(edx2$title, x))
}

howmany("Forrest Gump")
res <- sapply(mov, howmany)
res
