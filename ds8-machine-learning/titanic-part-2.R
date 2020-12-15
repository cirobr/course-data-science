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
set.seed(6, sample.kind = "Rounding") # if using R 3.5 or earlier, remove the sample.kind argument
