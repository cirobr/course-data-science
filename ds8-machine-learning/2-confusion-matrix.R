# tabulate each combination of prediction and actual value
table(predicted = y_hat, actual = test_set$sex)
test_set %>% 
  mutate(y_hat = y_hat) %>%
  group_by(sex) %>% 
  summarize(accuracy = mean(y_hat == sex))

# prevalence of men in dataset
prev <- mean(y == "Male")

#confusion matrix
confusionMatrix(data = y_hat, reference = test_set$sex)