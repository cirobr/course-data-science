#Comprehensive Assessment: Puerto Rico Hurricane Mortality
library(tidyverse)
library(pdftools)
options(digits = 3)    # report 3 significant digits

fn <- system.file("extdata", "RD-Mortality-Report_2015-18-180531.pdf", package="dslabs")
txt <- pdf_text(fn)

x <- str_split(txt[9], "\n")
s <- data.frame(x[1])
s[1,]
header_index <- str_which(x, "2015")
header_index
header_index <- 2

header <- s[header_index,]

tail_index <- 35

n <- str_count()

s1 <- s[-1,]
