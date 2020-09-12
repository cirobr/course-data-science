### load example matrix
library(tidyverse)
library(dslabs)
if(!exists("mnist")) mnist <- read_mnist()

class(mnist$train$images)

x <- mnist$train$images
y <- mnist$train$labels

options(digits = 3)

lower <- 50
lower <- lower + 1
upper <- 205
upper <- upper - 1
grey_area <- c(lower:upper)

mean(x %in% grey_area)