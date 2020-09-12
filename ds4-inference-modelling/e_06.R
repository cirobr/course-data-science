p <- seq(0, 1, length = 100)
sample_sizes <- c(25, 100, 1000)
for (N in sample_sizes) {
  se <- sqrt(p*(1-p)/N)
  plot(p, se, ylim = c(0, 0.1))
}