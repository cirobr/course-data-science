# calculate z to solve for 95% confidence interval
#p <- 0.95
p <- seq(0.01, 0.99, 0.01)

p_tail <- (1-p) / 2
p_tail

z <- qnorm(1 - p_tail)

#demonstrating symmetry of the confidence interval
z
qnorm(p_tail)

qnorm(p_tail) == - z

# demonstrating that qnorm gives the z value for a given probability
pnorm(qnorm(p))
pnorm(qnorm(p)) == p

# demonstrating that this z value gives correct probability for interval
p_int <- pnorm(z) - pnorm(-z)
p_int

plot(p, p_int)
abline(0,1)