# R version: 4.1.0
setwd("~/projects/data-science-course/ds9-capstone")

# clean memory
rm(edx)
rm(df)

# environment
library(ggplot2)
library(caret)
library(tidyverse)

options(digits = 3)

# read dataset from csv
if(!exists("edx2")) {edx2 <- read_csv(file = "./dat/edx2.csv")}
head(edx2)

# relevance of parameters
N = nrow(edx2)
remove_columns <- colnames(edx2)[1:3]
df <- edx2 %>% select(-remove_columns) %>% summarise(across(.cols = everything(), sum)) / N
df <- sort(df, decreasing = TRUE)
df <- data.frame(colnames(df), as.numeric(df))
colnames(df) <- c("Genres", "Proportion")
head(df)

df %>% ggplot(aes(Genres, Proportion)) + 
  geom_bar(stat = "identity") +
  ggtitle("Relevance of Film Genres") +
  geom_text(aes(Genres, Proportion, label = Proportion), size=2.5, angle = 90, hjust = -0.1) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  #scale_y_continuous(labels = scales::percent)
