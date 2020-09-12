library(dslabs)
data(movielens)
head(movielens)

sort(table(year(as_datetime(movielens$timestamp))), decreasing = TRUE)

sort(table(hour(as_datetime(movielens$timestamp))), decreasing = TRUE)
