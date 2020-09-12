x <- matrix(rnorm(100*10), 100, 10)

dim(x)
nrow(x)
ncol(x)

x_1 <- x + seq(nrow(x))
x_2 <- sweep(x, 1, 1:nrow(x),"+")
mean(x_1 == x_2)
