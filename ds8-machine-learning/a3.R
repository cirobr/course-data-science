options(digits = 3)

library(caret)
data(iris)
iris <- iris[-which(iris$Species=='setosa'),]
y <- iris$Species



#Q7
set.seed(2, sample.kind="Rounding")
# line of code
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
test <- iris[test_index,]
train <- iris[-test_index,]



#Q8
tab <- train %>% gather(key = "Species")
head(tab)

tab <- tab %>%
  group_by(Species) %>%
  summarize(mean = mean(value), sd = sd(value)) %>%
  mutate(cutoff = mean + 2 * sd)
tab

### Petal.Lenght
y_hat <- ifelse(train$Petal.Length > tab$cutoff[1], "virginica", "versicolor") %>%
  factor(levels = levels(test$Species))
mean(y_hat == train$Species)

cutoff <- seq(min(train$Petal.Length), max(train$Petal.Length), 0.1)

accuracy <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train$Petal.Length > x, "virginica", "versicolor") %>%
    factor(levels = levels(test$Species))
  mean(y_hat == train$Species)
})

data.frame(cutoff, accuracy) %>% 
  ggplot(aes(cutoff, accuracy)) + 
  geom_point() + 
  geom_line() 
max(accuracy)

best_cutoff <- cutoff[which.max(accuracy)]
best_cutoff
cutoff_pl <- best_cutoff

### Petal.Width
y_hat <- ifelse(train$Petal.Width > tab$cutoff[2], "virginica", "versicolor") %>%
  factor(levels = levels(test$Species))
mean(y_hat == train$Species)

cutoff <- seq(min(train$Petal.Width), max(train$Petal.Width), 0.1)

accuracy <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train$Petal.Width > x, "virginica", "versicolor") %>%
    factor(levels = levels(test$Species))
  mean(y_hat == train$Species)
})

data.frame(cutoff, accuracy) %>% 
  ggplot(aes(cutoff, accuracy)) + 
  geom_point() + 
  geom_line() 
max(accuracy)

best_cutoff <- cutoff[which.max(accuracy)]
best_cutoff
cutoff_pw <- best_cutoff

### Sepal.Lenght
y_hat <- ifelse(train$Sepal.Length > tab$cutoff[3], "virginica", "versicolor") %>%
  factor(levels = levels(test$Species))
mean(y_hat == train$Species)

cutoff <- seq(min(train$Sepal.Length), max(train$Sepal.Length), 0.1)

accuracy <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train$Sepal.Length > x, "virginica", "versicolor") %>%
    factor(levels = levels(test$Species))
  mean(y_hat == train$Species)
})

data.frame(cutoff, accuracy) %>% 
  ggplot(aes(cutoff, accuracy)) + 
  geom_point() + 
  geom_line() 
max(accuracy)

best_cutoff <- cutoff[which.max(accuracy)]
best_cutoff

### Sepal.Width
y_hat <- ifelse(train$Sepal.Width > tab$cutoff[4], "virginica", "versicolor") %>%
  factor(levels = levels(test$Species))
mean(y_hat == train$Species)

cutoff <- seq(min(train$Sepal.Width), max(train$Sepal.Width), 0.1)

accuracy <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train$Sepal.Width > x, "virginica", "versicolor") %>%
    factor(levels = levels(test$Species))
  mean(y_hat == train$Species)
})

data.frame(cutoff, accuracy) %>% 
  ggplot(aes(cutoff, accuracy)) + 
  geom_point() + 
  geom_line() 
max(accuracy)

best_cutoff <- cutoff[which.max(accuracy)]
best_cutoff



###Q9
# Chosen: Petal.Lenght
### Petal.Lenght
y_hat <- ifelse(train$Petal.Length > tab$cutoff[1], "virginica", "versicolor") %>%
  factor(levels = levels(test$Species))
mean(y_hat == train$Species)

cutoff <- seq(min(train$Petal.Length), max(train$Petal.Length), 0.1)

accuracy <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train$Petal.Length > x, "virginica", "versicolor") %>%
    factor(levels = levels(test$Species))
  mean(y_hat == train$Species)
})

max_accuracy <- max(accuracy)
max_accuracy
best_cutoff <- cutoff[which.max(accuracy)]
best_cutoff

y_hat <- ifelse(test$Petal.Length > best_cutoff, "virginica", "versicolor") %>%
  factor(levels = levels(test$Species))
mean(y_hat == test$Species)



#Q10

### Petal.Lenght
y_hat <- ifelse(test$Petal.Length > tab$cutoff[1], "virginica", "versicolor") %>%
  factor(levels = levels(test$Species))
mean(y_hat == test$Species)

cutoff <- seq(min(test$Petal.Length), max(test$Petal.Length), 0.1)

accuracy <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(test$Petal.Length > x, "virginica", "versicolor") %>%
    factor(levels = levels(test$Species))
  mean(y_hat == test$Species)
})

data.frame(cutoff, accuracy) %>% 
  ggplot(aes(cutoff, accuracy)) + 
  geom_point() + 
  geom_line() 
max(accuracy)

best_cutoff <- cutoff[which.max(accuracy)]
best_cutoff

### Petal.Width
y_hat <- ifelse(test$Petal.Width > tab$cutoff[2], "virginica", "versicolor") %>%
  factor(levels = levels(test$Species))
mean(y_hat == test$Species)

cutoff <- seq(min(test$Petal.Width), max(test$Petal.Width), 0.1)

accuracy <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(test$Petal.Width > x, "virginica", "versicolor") %>%
    factor(levels = levels(test$Species))
  mean(y_hat == test$Species)
})

data.frame(cutoff, accuracy) %>% 
  ggplot(aes(cutoff, accuracy)) + 
  geom_point() + 
  geom_line() 
max(accuracy)

best_cutoff <- cutoff[which.max(accuracy)]
best_cutoff

### Sepal.Lenght
y_hat <- ifelse(test$Sepal.Length > tab$cutoff[3], "virginica", "versicolor") %>%
  factor(levels = levels(test$Species))
mean(y_hat == test$Species)

cutoff <- seq(min(test$Sepal.Length), max(test$Sepal.Length), 0.1)

accuracy <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(test$Sepal.Length > x, "virginica", "versicolor") %>%
    factor(levels = levels(test$Species))
  mean(y_hat == test$Species)
})

data.frame(cutoff, accuracy) %>% 
  ggplot(aes(cutoff, accuracy)) + 
  geom_point() + 
  geom_line() 
max(accuracy)

best_cutoff <- cutoff[which.max(accuracy)]
best_cutoff

### Sepal.Width
y_hat <- ifelse(test$Sepal.Width > tab$cutoff[4], "virginica", "versicolor") %>%
  factor(levels = levels(test$Species))
mean(y_hat == test$Species)

cutoff <- seq(min(test$Sepal.Width), max(test$Sepal.Width), 0.1)

accuracy <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(test$Sepal.Width > x, "virginica", "versicolor") %>%
    factor(levels = levels(test$Species))
  mean(y_hat == test$Species)
})

data.frame(cutoff, accuracy) %>% 
  ggplot(aes(cutoff, accuracy)) + 
  geom_point() + 
  geom_line() 
max(accuracy)

best_cutoff <- cutoff[which.max(accuracy)]
best_cutoff



#Q11
y_hat <- ifelse(test$Petal.Length > cutoff_pl |
                test$Petal.Width > cutoff_pw, "virginica", "versicolor") %>%
  factor(levels = levels(test$Species))
mean(y_hat == test$Species)

