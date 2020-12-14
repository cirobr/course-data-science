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


# q1
y <- titanic_clean$Survived

# generate training and test sets
set.seed(42, sample.kind = "Rounding") # if using R 3.5 or earlier, remove the sample.kind argument
test_index <- createDataPartition(y, times = 1, p = 0.2, list = FALSE)
test_set <- titanic_clean[test_index, ]
train_set <- titanic_clean[-test_index, ]

# observações em test_set
dim(test_set)[1]

# train_set
dim(train_set)[1]

# proporção de sobreviventes
mean(train_set$Survived == 1)


# q2
set.seed(3, sample.kind = "Rounding") # if using R 3.5 or earlier, remove the sample.kind argument
y_hat <- sample(c(0, 1), length(test_index), replace = TRUE)
#accuracy
mean(test_set$Survived == y_hat)


# q3a
# proportion of survivals by gender
s_f <- train_set %>% summarise(Survived, Sex) %>% filter(Sex == "female") %>% table() %>% prop.table()
s_f[2]
s_m <- train_set %>% summarise(Survived, Sex) %>% filter(Sex == "male") %>% table() %>% prop.table()
s_m[2]

# q3b
# predicting survival by sex
survival_rate <- c(s_m[2], s_f[2])

surv <- function(sex){
  y_hat <- ifelse(sex == "male" & survival_rate[1] > 0.5, 1,
                  ifelse(sex == "female" & survival_rate[2] > 0.5, 1, 0))
  y_hat
}

y_hat <- sapply(test_set$Sex, surv)
mean(test_set$Survived == y_hat)


# q4a
# passenger classes more likely to survive
df <- train_set %>% summarise(Pclass, Survived) %>% table()
df
df <- t(apply(as.matrix(df), 1, prop.table))
df

# q4b
# predict survival by passenger class
survival_rate <- df[,2]

surv2 <- function(pcl){
  y_hat <- ifelse(pcl == 1 & survival_rate[1] > 0.5, 1,
                  ifelse(pcl == 2 & survival_rate[2] > 0.5, 1,
                         ifelse(pcl == 3 & survival_rate[3] > 0.5, 1, 0)))
  y_hat
}

y_hat <- sapply(test_set$Pclass, surv2)
mean(test_set$Survived == y_hat)

# q4c
# sex-class combination more likely to survive
df <- train_set %>% 
  group_by(Pclass, Sex, Survived)
            