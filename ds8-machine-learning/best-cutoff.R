options(digits = 3)

library(caret)
data(iris)
iris <- iris[-which(iris$Species=='setosa'),]
y <- iris$Species

# Split train/test sets
set.seed(2, sample.kind="Rounding")
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
test <- iris[test_index,]
train <- iris[-test_index,]

# Calculates table with individual cutoffs
tab <- train %>% gather(key = "Species")
head(tab)

tab <- tab %>%
  group_by(Species) %>%
  summarize(mean = mean(value), sd = sd(value)) %>%
  mutate(cutoff = mean + 2 * sd)
tab

### Find the best cutoff for Petal.Lenght
y_hat <- ifelse(train$Petal.Length > tab$cutoff[1], "virginica", "versicolor") %>%
  factor(levels = levels(test$Species))
mean(y_hat == train$Species)

min_cutoff <- round(min(train$Petal.Length), digits = 1)
max_cutoff <- round(max(train$Petal.Length), digits = 1)
cutoff <- seq(min_cutoff, max_cutoff, 0.1)

accuracy <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train$Petal.Length > x, "virginica", "versicolor") %>%
    factor(levels = levels(test$Species))
  mean(y_hat == train$Species)
})

data.frame(cutoff, accuracy) %>% 
  ggplot(aes(cutoff, accuracy)) + 
  geom_point() + 
  geom_line() 

max_accuracy <- max(accuracy)
max_accuracy

best_cutoff <- cutoff[which.max(accuracy)]
best_cutoff
