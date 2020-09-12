library(dslabs)
library(tidyverse)
library(caret)
options(digits = 3)

data(mnist_27)
# set.seed(1995) # if R 3.5 or earlier
set.seed(1995, sample.kind="Rounding") # if R 3.6 or later
indexes <- createResample(mnist_27$train$y, 10)



# Q1
rs1 <- indexes$Resample01 %>% table()
head(rs1,10)
rs1[c("3", "4", "7")]



#Q2
i <- 1
temp <- indexes[i] %>% table()
temp["3"]

check3 <- function(lista){
  temp <- lista %>% table()
  temp["3"]
}

sapply(indexes, check3)
sum(sapply(indexes, check3), na.rm = TRUE)



# Q3
y <- rnorm(100, 0, 1)
qnorm(0.75)   #75th quantile
quantile (y, 0.75)

B <- 10000
set.seed(1, sample.kind="Rounding") # if R 3.6 or later
y_i <- replicate(B, rnorm(100,0,1))

# function quantile 75
q75 <- function(y){
  quantile(y, 0.75)
}

q <- apply(y_i, MARGIN = 2, q75)
c(mean(q), sd(q))


# Q4
# set.seed(1) # # if R 3.5 or earlier
set.seed(1, sample.kind = "Rounding") # if R 3.6 or later
y <- rnorm(100, 0, 1)

n_samples <- 10
set.seed(1, sample.kind = "Rounding") # if R 3.6 or later
tbl <- createResample(y, n_samples, list = TRUE)

# um resample
ind_i <- tbl[1]
ind_i <- as.vector(unlist(ind_i))     # change list to vector
y_ind_i <- y[ind_i]

# todos os resamples
ind <- matrix(unlist(tbl),length(y), n_samples)     # change list to matrix
y_ind <- matrix(y[ind], length(y), n_samples)
q <- apply(y_ind, MARGIN = 2, q75)
c(mean(q), sd(q))


# Q5
n_samples <- 10000
set.seed(1, sample.kind = "Rounding") # if R 3.6 or later
tbl <- createResample(y, n_samples, list = TRUE)

# todos os resamples
ind <- matrix(unlist(tbl),length(y), n_samples)     # change list to matrix
y_ind <- matrix(y[ind], length(y), n_samples)
q <- apply(y_ind, MARGIN = 2, q75)
c(mean(q), sd(q))
