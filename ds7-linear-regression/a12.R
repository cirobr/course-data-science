#dados de times
dat <- cbind(c(1, 2, 4, 1, 0, 1), c(1, 1, 6, 2, 1, 0))

rownames(dat) <- c("intercept", "BB", "S", "D", "T", "HR")
#dat <- t(dat)
dat <- data.frame(dat)
dat
c(class(dat[,1]), class(dat[,2]))

# regression with BB, singles, doubles, triples, HR
fit <- Teams %>% 
  filter(yearID %in% 1961:2001) %>% 
  mutate(BB = BB / G, 
         singles = (H - X2B - X3B - HR) / G, 
         doubles = X2B / G, 
         triples = X3B / G, 
         HR = HR / G,
         R = R / G) %>%  
  lm(R ~ BB + singles + doubles + triples + HR, data = .)
coefs <- tidy(fit, conf.int = TRUE)
coefs

# regression line equation
sum(coefs$estimate * dat[,1])
sum(coefs$estimate * dat[,2])
