dat_wide <- c("Alabama",1990,4040587,86,19,76,1,
              "Alabama",1991,4066003,39,14,65,0,
              "Alabama",1992,4097169,35,12,24,0,
              "Alabama",1993,4133242,40,22,67,0,
              "Alabama",1994,4173361,72,12,39,0,
              "Alabama",1995,4216645,75,2,38,0)
dat_wide <- matrix(dat_wide,
              nrow=6,
              ncol=7,
              byrow=TRUE)
colnames(dat_wide) <- c("state", "year", "population", "HepatitisA", "Mumps", "Polio", "Rubella")

dat_wide <- as.data.frame(dat_wide)
dat_wide

dat_tidy <- dat_wide %>%
  gather(key = disease, value = count, HepatitisA:Rubella)
dat_tidy
