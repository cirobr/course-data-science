# titanic starting point
library(titanic)    # loads titanic_train data frame
library(caret)
library(tidyverse)
library(rpart)

# 3 significant digits
options(digits = 3)

# clean the data - `titanic_train` is loaded with the titanic package
titanic_clean <- titanic_train %>%
  mutate(Survived = factor(Survived),
         Embarked = factor(Embarked),
         Age = ifelse(is.na(Age), median(Age, na.rm = TRUE), Age), # NA age to median age
         FamilySize = SibSp + Parch + 1) %>%    # count family members
  select(Survived,  Sex, Pclass, Age, Fare, SibSp, Parch, FamilySize, Embarked)

# generate training and test sets
y <- titanic_clean$Survived
set.seed(42, sample.kind = "Rounding") # if using R 3.5 or earlier, remove the sample.kind argument
test_index <- createDataPartition(y, times = 1, p = 0.2, list = FALSE)
test_set <- titanic_clean[test_index, ]
train_set <- titanic_clean[-test_index, ]


# q7 qda lda prediction
set.seed(1, sample.kind = "Rounding") # if using R 3.5 or earlier, remove the sample.kind argument
train_qda <- train(Survived ~ Fare, method = "qda", data = train_set)
y_hat_qda <- predict(train_qda, test_set)
mean(y_hat_qda == test_set$Survived)

set.seed(1, sample.kind = "Rounding") # if using R 3.5 or earlier, remove the sample.kind argument
train_lda <- train(Survived ~ Fare, method = "lda", data = train_set)
y_hat_lda <- predict(train_qda, test_set)
mean(y_hat_lda == test_set$Survived)


# q8 glm prediction
set.seed(1, sample.kind = "Rounding") # if using R 3.5 or earlier, remove the sample.kind argument
train_glm <- train(Survived ~ Age, method = "glm", data = train_set)
y_hat_glm <- predict(train_glm, test_set)
mean(y_hat_glm == test_set$Survived)

set.seed(1, sample.kind = "Rounding") # if using R 3.5 or earlier, remove the sample.kind argument
train_glm <- train(Survived ~ Sex + Pclass + Fare + Age, method = "glm", data = train_set)
y_hat_glm <- predict(train_glm, test_set)
mean(y_hat_glm == test_set$Survived)

set.seed(1, sample.kind = "Rounding") # if using R 3.5 or earlier, remove the sample.kind argument
train_glm <- train(Survived ~ ., method = "glm", data = train_set)
y_hat_glm <- predict(train_glm, test_set)
mean(y_hat_glm == test_set$Survived)


# q9 knn prediction
#getModelInfo("knn")
#modelLookup("knn")
set.seed(6, sample.kind = "Rounding") # if using R 3.5 or earlier, remove the sample.kind argument
train_knn <- train(Survived ~ .,
                   method = "knn",
                   data = train_set,
                   tuneGrid = data.frame(k = seq(3, 51, 2)))
ggplot(train_knn, highlight = TRUE)
train_knn$bestTune

y_hat_knn <- predict(train_knn, test_set)
mean(y_hat_knn == test_set$Survived)

confusionMatrix(y_hat_knn, test_set$Survived)$overall[["Accuracy"]]


# q10 knn 10-fold cross-validation
set.seed(8, sample.kind = "Rounding") # if using R 3.5 or earlier, remove the sample.kind argument

control <- trainControl(method = "cv",   # stands for cross validation
                        number = 10,     # 10-fold cross validation
                        p = .9)          # 90% of data as train set

train_knn_cv <- train(Survived ~ .,
                      method = "knn", 
                      data = train_set,
                      tuneGrid = data.frame(k = seq(3, 51, 2)),
                      trControl = control)
ggplot(train_knn_cv, highlight = TRUE)
train_knn_cv$bestTune

y_hat_knn_cv <- predict(train_knn_cv, test_set)
confusionMatrix(y_hat_knn_cv, test_set$Survived)$overall[["Accuracy"]]


# q11-a decision tree method
set.seed(10, sample.kind = "Rounding") # if using R 3.5 or earlier, remove the sample.kind argument

train_rpart <- train(Survived ~ .,
                     method = "rpart",
                     tuneGrid = data.frame(cp = seq(0, 0.05, 0.002)),
                     data = train_set)
ggplot(train_rpart, highlight = TRUE)
train_rpart$bestTune

y_hat_rpart <- predict(train_rpart, test_set)
confusionMatrix(y_hat_rpart, test_set$Survived)$overall[["Accuracy"]]

# q11-b plot the decision tree
plot(train_rpart$finalModel, margin = 0.1)
text(train_rpart$finalModel, cex = 0.75)

# q11-c predict about a number of passengers
Sex <- c("male", "female", "female", "male", "female", "female", "male")
Age <- c(28, 0, 0, 5, 0, 17, 17)
Pclass <- c(0, 2, 3, 0, 3, 1, 1)
Fare <- c(0, 0, 8, 0,25, 0, 0) 
SibSp <- c(0, 0, 0, 4, 0, 2 ,2)
Parch <- 0
FamilySize <- 0
Embarked <- ""

dat <- data.frame(Sex, Age, Pclass, Fare, SibSp, Parch, FamilySize, Embarked)

y_hat_rpart2 <- predict(train_rpart, dat)
y_hat_rpart2


# q12 random forest model
set.seed(14, sample.kind = "Rounding") # if using R 3.5 or earlier, remove the sample.kind argument

train_rf <- train(Survived ~ .,
                  method = "rf",
                  tuneGrid = data.frame(mtry = seq(1:7)),
                  ntree = 100,
                  data = train_set)
ggplot(train_rf, highlight = TRUE)
train_rf$bestTune

y_hat_rf <- predict(train_rf, test_set)
confusionMatrix(y_hat_rf, test_set$Survived)$overall[["Accuracy"]]
mean(y_hat_rf == test_set$Survived)

imp <- varImp(train_rf)
imp
