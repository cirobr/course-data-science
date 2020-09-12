library(tidyverse)
library(caret)
library(dslabs)
data(heights)
options(digits = 3)

# define the predictor and outcome
x <- heights$height
y <- as.factor(heights$sex)

# generate training and test sets
set.seed(1, sample.kind = "Rounding") # if using R 3.5 or earlier, remove the sample.kind argument
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
train_set <- heights[-test_index, ]
test_set <- heights[test_index, ]

# knn single
k <- 5
x_knn <- as.matrix(train_set$height)                              # predictor do trainset
y_knn <- train_set$sex                                            # outcome do train dataset
knn_fit <- knn3(x_knn, y_knn, k = k)                              # chamada da fun??o knn para calculo dos coeficientes

y_hat_knn <- predict(knn_fit, test_set$height, type = "class")    # calcula estimativa com base no knn_fit e test_set
mean(y_hat_knn == test_set$sex)                                   # verifica??o da estimativa: calcula m?dia de acerto
F_meas(data = y_hat_knn, reference = factor(test_set$sex))        # calcula F1-score

# knn vector
k <- seq(1, 103, 3)
f1_vec <- sapply(k, function(k){
  knn_fit <- knn3(x_knn, y_knn, k = k)                              # chamada da fun??o knn para calculo dos coeficientes
  y_hat_knn <- predict(knn_fit, test_set$height, type = "class")    # calcula estimativa com base no knn_fit e test_set
  F_meas(data = y_hat_knn, reference = factor(test_set$sex))        # calcula F1-score
})

max(f1_vec)
which.max(f1_vec)
k[which.max(f1_vec)]
