#Heart Disease Prediction Project
#University of California, Irvine Machine Learning Repository
#Heart Disease Data Set


#importing libraries
if(!require(tidyverse)) install.packages("tidyverse")
if(!require(caret)) install.packages("dplyr")
if(!require(dplyr)) install.packages("dplyr")
if(!require(matrixStats)) install.packages("matrixStats")
if(!require(gam)) install.packages("gam")
if(!require(evtree)) install.packages("evtree")

library(tidyverse)
library(caret)
library(dplyr)
library(matrixStats)
library(gam)
library(evtree)

#importing the University of California, Irvine Heart Disease Data set
heart <- read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/heart-disease/processed.cleveland.data",
                  header=FALSE,sep=",",na.strings = '?')
names(heart) <- c( "age", "sex", "cp", "trestbps", "chol","fbs", "restecg",
                        "thalach","exang", "oldpeak","slope", "ca", "thal", "num")

#Data Cleaning

#first we are going to take a look at the data set and get an idea of what we
#need to clean and how the data set is structured 
head(heart)
dim(heart)
str(heart)
summary(heart)

#The feature num is the angiographic disease status. 0 represents no heart disease 
#while 1-4 represents the extent of heart disease in the patient. So for num, 
#we are going to convert anything greater than 0 to equal 1, leaving us with
#0 (no heart disease) and 1 (heart disease)
heart$num[heart$num > 0] <- 1

#check to make sure 1-4 were converted to 1
summary(heart$num)

#checking to see if the changes were made correctly
heart$sex

#check for any NAs
sum(is.na(heart) == TRUE)

#we have 6 NAs which is not too many so we will delete those rows
heart <- na.omit(heart)
dim(heart)

#Let's look at a summary of the data with just the observations without heart disease
heart %>% filter(num == 0) %>% summary()

#Now let's take a look at a summary of the data with just the observations with heart disease
heart %>% filter(num == 1) %>% summary()

#box plot
boxplot(heart)

#from the box plot, 3 features stand out to me that we will look at in the next
#few graphs: tresbps, chol, thalach

#Scatter plot of resting blood pressure and age
heart %>% ggplot(aes(age,trestbps)) +
  geom_point(aes(color = factor(num))) +
  ggtitle("Resting Blood Presure and Age") +
  ylab("Resting BP") +
  scale_color_manual(name = "Heart Disease",
                      labels = c("Negitive","Positive"),
                     values = c("blue","red"))
  
#Scatter plot of serum cholestoral and age
heart %>% ggplot(aes(age,chol)) +
  geom_point(aes(color = factor(num))) +
  ggtitle("Serum Cholestoral and Age") +
  ylab("Cholestoral") +
  scale_color_manual(name = "Heart Disease",
                     labels = c("Negitive","Positive"),
                     values = c("blue","red"))
  
#Scatter plot of maximum heart rate achieved and age
heart %>% ggplot(aes(age,thalach)) +
  geom_point(aes(color = factor(num)))+
  ggtitle("Maximum Heart Rate Achieved and Age") +
  ylab("Max Heart Rate") +
  scale_color_manual(name = "Heart Disease",
                     labels = c("Negitive","Positive"),
                     values = c("blue","red"))

#Scatter plot of maximum heart rate achieved, age and sex
#0 = female and 1 = male
heart %>% ggplot(aes(age,thalach)) +
  geom_point(aes(color = factor(num))) +
  facet_grid(.~sex) +
  ggtitle("Maximum Heart Rate Achieved by Age and Sex") +
  ylab("Max Heart Rate") +
  scale_color_manual(name = "Heart Disease",
                     labels = c("Negitive","Positive"),
                     values = c("blue","red"))

#bar chart of distribution of heart disease split by sex
heart %>% ggplot(aes(num)) +
  geom_bar(aes(fill=factor(num))) +
  facet_grid(.~sex) +
  ggtitle("Distribution of Heart Disease by Sex") +
  scale_color_manual(name = "Heart Disease",
                     labels = c("Negitive","Positive"),
                     values = c("blue","red"))

#scatter plot of maximum heart rate achieved, cholesterol, sex
heart %>% ggplot(aes(thalach,chol)) +
  geom_point(aes(col=factor(num))) +
  facet_grid(.~sex) +
  labs(title = "Maximum Heart Rate Achieved, Cholesterol and Sex",
       x = "Max Heart Rate", y = "Cholesterol") +
  scale_color_manual(name = "Heart Disease",
                     labels = c("Negitive","Positive"),
                     values = c("blue","red"))

#bar chart of number of major vessels (0-3) colored by flourosopy during exam
#split by Heart Disease diagnosis
heart %>% ggplot(aes(ca)) +
  geom_bar(aes(fill=factor(num))) +
  facet_grid(.~num) +
  ggtitle("Number of Major Vessels (0-3) Colored by Flourosopy During Exam
Split by Heart Disease Diagnosis") +
  xlab("Major Vessels") +
  scale_color_manual(name = "Heart Disease",
                     labels = c("Negitive","Positive"),
                     values = c("blue","red"))

#Analysis

#creating variable x which will consist of the data set expect the feature we are
#trying to predict
x <- heart[,-14]

#creating variable y which will consist the feature we are trying to predict
y <- heart$num

#We will split these two up into training and test
set.seed(10,sample.kind = "Rounding")
test_index <- createDataPartition(y,times = 1, p=.2,list = FALSE)
test_x <- x[test_index,]
test_y <- y[test_index]
train_x <- x[-test_index,]
train_y <- y[-test_index]

#checking to see if the proportions are the same for the train and test set
mean(test_y == 0)
mean(train_y == 0)

#Will be using k-fold cross validation on all the algorithms
#creating the k-fold parameters, k is 10
control <- trainControl(method = "cv", number = 10, p = .9)

#first model
#Logistic regression model

#training the model using train set
train_glm <- train(train_x, as.factor(train_y), method = "glm",
                   family = "binomial",
                   trControl = control)

#creating the predictions
glm_preds <- predict(train_glm, test_x)

#getting the overall accuracy
logistic_regression <- confusionMatrix(glm_preds,as.factor(test_y))$overall[["Accuracy"]]

#viewing accuracy results
logistic_regression


#Linear discriminant analysis

#training the model using the train set
train_lda <- train(train_x, as.factor(train_y), method = "lda",
                   trControl = control)

#creating the predictions
lda_preds <- predict(train_lda, test_x)

#getting the overall accuracy
LDA <- confusionMatrix(lda_preds,as.factor(test_y))$overall[["Accuracy"]]

#viewing accuracy results
LDA


#Quadratic discriminant analysis

#training the model using the train set
train_qda <- train(train_x, as.factor(train_y), method = "qda",
                   trControl = control)

#creating the predictions
qda_preds <- predict(train_qda, test_x)

#getting the overall accuracy
QDA <- confusionMatrix(qda_preds,as.factor(test_y))$overall[["Accuracy"]]

#viewing accuracy results
QDA


#Loess model


#creating grid for the two parameter: span, degree
grid <- expand.grid(span = seq(.1,1, len = 10), degree = 1)

#setting the seed  
set.seed(5, sample.kind = "Rounding")

#training the model using the training set
train_loess <- train(train_x, as.factor(train_y), method = "gamLoess",
                     trControl = control,
                     tuneGrid = grid)

#creating graph of the tuning result
ggplot(train_loess, highlight = TRUE) +
  ggtitle("Loess Model")

#creating the predictions
loess_preds <- predict(train_loess, test_x)

#getting the accuracy
Loess <- confusionMatrix(loess_preds,as.factor(test_y))$overall[["Accuracy"]]

#viewing accuracy result
Loess


#K-nearest neighbors model

#setting the seed
set.seed(7, sample.kind = "Rounding")

#creating tuning parameter for K
tuning <- data.frame(k = seq(2,200,2))

#training the model with the training set
train_knn <- train(train_x, as.factor(train_y),method = "knn",
                   trControl = control,
                   tuneGrid = tuning,)

#creating graph of tuning result
ggplot(train_knn, highlight = TRUE) + 
  ggtitle("K-Nearest Neighbor")

#finding best tuning
train_knn$bestTune

#creating prediction
knn_preds <- predict(train_knn, test_x)

#getting accuracy
Knearest_neighbors <- confusionMatrix(knn_preds,as.factor(test_y))$overall[["Accuracy"]]

#viewing accuracy result
Knearest_neighbors


#Random Forest


#setting the seed
set.seed(9, sample.kind = "Rounding")

#setting the tuning parameters
tuning <- data.frame(mtry = seq(1,25,2))

#training the model using the train set
train_rf <- train(train_x, as.factor(train_y),method = "rf",
                  tuneGrid = tuning,
                  trControl = control,
                  importance = TRUE)

#creating graph of tuning results
ggplot(train_rf, highlight = TRUE) +
  ggtitle("Random Forest")

#finding the best tuning result
train_rf$bestTune

#creating predictions
rf_preds <- predict(train_rf, test_x)

#getting accuracy
Random_Forest <- confusionMatrix(rf_preds,as.factor(test_y))$overal[["Accuracy"]]

#viewing accuracy result
Random_Forest

#viewing importance
varImp(train_rf)


#Tree Models from Genetic Algorithms

#setting the seed
set.seed(7, sample.kind = "Rounding")

#setting the tuning parameter alpha
tuning <- data.frame(alpha = seq(2,50,2))

#training the model on the train set
train_tree <- train(train_x, as.factor(train_y),method = "evtree",
                  tuneGrid = tuning,
                  trControl = control)

#creating a graph for the tuning results
ggplot(train_tree, highlight = TRUE) +
  ggtitle("Tree Models From Genetic Algorithms")

#finding best tune
train_tree$bestTune

#creating predictions
tree_preds <- predict(train_tree, test_x)

#getting accuracy results
tree_model <- confusionMatrix(tree_preds,as.factor(test_y))$overall[["Accuracy"]]

#viewing accuracy results
tree_model


#Least Squares Support Vector Machine

#setting the seed
set.seed(7, sample.kind = "Rounding")

#setting the tuning parameter alpha
tuning <- data.frame(tau = seq(2,100,2))

#training the model on the train set
train_SVM <- train(train_x, as.factor(train_y),method = "lssvmLinear",
                    tuneGrid = tuning,
                    trControl = control)

#creating a graph for the tuning results
ggplot(train_SVM, highlight = TRUE) +
  ggtitle("Least Squares Support Vector Machine")

#finding best tune
train_SVM$bestTune

#creating predictions
SVM_preds <- predict(train_SVM, test_x)

#getting accuracy results
SVM <- confusionMatrix(SVM_preds,as.factor(test_y))$overall[["Accuracy"]]

#viewing accuracy results
SVM


#Bayesian Generalized Linear Model

#setting the seed
set.seed(7, sample.kind = "Rounding")

#training the model on the train set
train_nb <- train(train_x, as.factor(train_y),method = "bayesglm",
                   trControl = control)

#creating predictions
nb_preds <- predict(train_nb, test_x)

#getting accuracy results
nb <- confusionMatrix(nb_preds,as.factor(test_y))$overall[["Accuracy"]]

#viewing accuracy results
nb


#Neural Network

#setting the seed
set.seed(7, sample.kind = "Rounding")

#setting the tuning parameter alpha
tuning <- data.frame(size = seq(10), decay = seq(1,5,1))

#training the model on the train set
train_nn <- train(train_x, as.factor(train_y),method = "nnet",
                   tuneGrid = tuning,
                   trControl = control)

#creating a graph for the tuning results
ggplot(train_nn, highlight = TRUE) +
  ggtitle("Neural Network")

#finding best tune
train_nn$bestTune

#creating predictions
nn_preds <- predict(train_nn, test_x)

#getting accuracy results
nn <- confusionMatrix(nn_preds,as.factor(test_y))$overall[["Accuracy"]]

#viewing accuracy results
nn


#Results

#creating a results table
result_table <- data.frame(Algorithm = c("Logistic Regression",
           "Linear Discriminant Analysis",
           "Ouadratic Discriminant Analysis",
           "Loess Model",
           "K-Nearest Neighbors",
           "Random Forest",
           "Tree Models from Genetic Algorithms",
           "Least Squares Support Vector Machine",
           "Bayesian Generalized Linear Model",
           "Neural Network"),
           Result = c(round(logistic_regression,2),
           round(LDA,2),
           round(QDA,2),
           round(Loess,2),
           round(Knearest_neighbors,2),
           round(Random_Forest,2),
           round(tree_model,2),
           round(SVM,2),
           round(nb,2),
           round(nn,2)))

#viewing final results
result_table