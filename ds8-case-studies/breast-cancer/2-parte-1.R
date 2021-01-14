# q1
length(y)
dim(x)[2]
mean(y == "M")
which.max(colMeans(x))
which.min(colSds(x))

#q2
d <- dim(x)[2]
m <- colMeans(x)
s <- colSds(x)
sweep(x, 2, 1:d, mean, FUN = "-")
