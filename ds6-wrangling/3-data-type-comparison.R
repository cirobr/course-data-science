# data type analysis of "murders"
library(dslabs)
data("murders")
class(murders)
str(murders)

head(murders)

# example of factors
levels(murders$region)

# filename is defined in the previous video
filename <- "murders.csv"

#this is a tibble
datt <- read_csv(filename)
class(datt)

# this is a data frame
datf <- read.csv(filename)
class(datf)

#however, they match to each other (mean == 1)
mean(datt == datf) == 1