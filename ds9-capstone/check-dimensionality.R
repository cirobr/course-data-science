# write datasets to csv
setwd("~/projects/data-science-course/ds9-capstone")

# libraries
library(ggplot2)
library(caret)
library(tidyverse)
options(digits=3)

# read edx dataset from csv
if(!exists("edx2")) {edx2 <- read_csv(file = "./dat/edx2.csv")}
head(edx2)

nr <- nrow(edx2)
df <- edx2 %>% select(-c(userId, movieId, rating)) %>% colSums() /nr * 100
df <- tibble(Genres=names(df), Percentage=df)
df <- df %>% arrange(desc(Percentage))
df

# removing predictors < 4%
edx3 <- edx2 %>% select(-c("Western", "Film-Noir", "Documentary", "IMAX", "No-Genre"))
nr <- nrow(edx3)
df <- edx3 %>% select(-c(userId, movieId, rating)) %>% colSums() /nr * 100
df <- tibble(Genres=names(df), Percentage=df)
df <- df %>% arrange(desc(Percentage))
df
edx2 <- edx3
rm(edx3)
