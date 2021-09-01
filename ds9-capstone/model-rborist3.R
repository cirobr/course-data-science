# environment
print("setup environment")
library(Rborist)                # parallel computing

# prepare dataset
head(df_train)
df_train <- df_train %>%
  select(-c(rating, timestamp, timestampYear, yearOfRelease))

# scale predictors and factor the outcome
df_train[-1]    <- scale(df_train[-1])
df_test[-1]     <- scale(df_test[-1])

# fit the model
print("fit Rborist model")
modelLookup("Rborist")

control <- trainControl(method = "cv",
                        number = 10,
                        p = .9,
                        verboseIter = TRUE)

numberOfPredictors <- ncol(df_train) - 1
maxGrid <- floor(numberOfPredictors / 2)
gridSearch <- expand.grid(predFixed = seq(2, maxGrid, by = 3),
                          minNode   = seq(500, 750, 1000)
)

set.seed(1, sample.kind = "Rounding")
rborist_fit <- df_train %>%
  train(unbiasedRating ~ .,
        method = "Rborist",
        data = .,
        nTree=100,
        #classWeight = classWeights,
        tuneGrid = gridSearch,
        trControl = control
        )

# load pre-built model to save execution time
# load(file="./mdl/rborist_fit.RData")

ggplot(rborist_fit, highlight = TRUE)
rborist_fit$bestTune
rborist_fit$finalModel

# predict the outcome
print("predict the outcome")
predicted <- predict(rf_rborist, df_test)
# predictedNonFactor <- varhandle::unfactor(predicted)

# calculate error metrics
print("calculate error metrics")
err <- errRMSE(test_set$rating, predicted)
err
hist(predicted)

# save model
# print("save model")
# save(rborist_fit, file="./mdl/rborist_fit.RData")

# clean memory
# rm(rborist_fit, df, df_train, df_test)
