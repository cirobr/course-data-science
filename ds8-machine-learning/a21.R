library(tidyverse)
library(caret)
options(digits = 5)

library(dslabs)
data(tissue_gene_expression)

dim(tissue_gene_expression$x)
table(tissue_gene_expression$y)

x <- tissue_gene_expression$x
y <- tissue_gene_expression$y

# generate training and test sets
set.seed(1, sample.kind = "Rounding") # if using R 3.5 or earlier, remove the sample.kind argument
test_index  <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)

x_train_set <- tissue_gene_expression$x[-test_index,]
y_train_set <- tissue_gene_expression$y[-test_index]

x_test_set  <- tissue_gene_expression$x[test_index,]
y_test_set  <- tissue_gene_expression$y[test_index]

# knn
x_knn <- as.matrix(x_train_set)                              # predictor do trainset
y_knn <- y_train_set                                         # outcome do train dataset

k = seq(1,7,2)
#k = 1

acc_vec <- sapply(k, function(k){
  fit <- train(x, y, method = "knn")
  fit$results$Accuracy
})

acc_vec
max(acc_vec)
which.max(acc_vec)
