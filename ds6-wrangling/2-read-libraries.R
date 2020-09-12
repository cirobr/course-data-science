library(dslabs)
library(tidyverse)    # includes readr
library(readxl)

# inspect the first 3 lines of original csv file
read_lines("murders.csv", n_max = 3)

# read file in CSV format from local folder
filename <- "murders.csv"
dat <- read_csv(filename)

#read using full path from dslabs library
path <- system.file("extdata", package="dslabs")
fullpath <- file.path(path, filename)
dat <- read_csv(fullpath)
head(dat)


#Ex: using functions from Basic R (creating data frames)
path <- system.file("extdata", package = "dslabs")
files <- list.files(path)
files

filename <- "murders.csv"
filename1 <- "life-expectancy-and-fertility-two-countries-example.csv"
filename2 <- "fertility-two-countries-example.csv"
datf=read.csv(file.path(path, filename))
dat1=read.csv(file.path(path, filename1))
dat2=read.csv(file.path(path, filename2))
