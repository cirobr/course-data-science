options(digits = 3)
# q1

# select 10 different models for prediction
models <- c("glm", "lda", "naive_bayes", "svmLinear", "knn", "gamLoess", "multinom", "qda", "rf", "adaboost")

# train all the above models
library(caret)
library(dslabs)
library(tidyverse)
# set.seed(1) # if using R 3.5 or earlier
set.seed(1, sample.kind = "Rounding") # if using R 3.6 or later
data("mnist_27")

fits <- lapply(models, function(model){ 
  print(model)
  train(y ~ ., method = model, data = mnist_27$train)
}) 

names(fits) <- models


# q2
# create a matrix of predictions for the testset
mat <- sapply(fits, function(fit){
  print(fit)
  predict(fit, mnist_27$test)
})


# q3
# overall accuracy
cm <- apply(mat, 2, function(model_pred){
  confusionMatrix(factor(model_pred), mnist_27$test$y)$overall[["Accuracy"]]
})
mean(cm)


# q4
# build an ensemble prediction
y_pred <- apply(mat, 1, function(c){
  avg <- mean(c == 7)
  ifelse(avg > 0.5, "7", "2")
})

overall_accuracy <- mean(y_pred == mnist_27$test$y)
overall_accuracy


# q5
sum(cm > overall_accuracy)
which(cm > overall_accuracy)


# q6
acc <- sapply(fits, function(fit){
  min(fit$results$Accuracy)
})
mean(acc)


# q7
