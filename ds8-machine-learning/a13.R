library(broom)
mnist_27$train %>% glm(y ~ x_2, family = "binomial", data = .) %>% tidy()
qplot(x_2, y, data = mnist_27$train)


dat <- mnist_27$train
total_range <- diff(range(dat$x_2))
span <- total_range * 0.5

fit <- loess(as.numeric(y) ~ x_2, degree = 1, span = span, data = dat)
fit

dat %>% mutate(smooth = fit$fitted) %>%
  ggplot(aes(x_2, y)) +
  geom_point(size = 3, alpha = .5, color = "grey") + 
  geom_line(aes(x_2, smooth), color="red")
