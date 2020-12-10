library(tidyverse)
library(dslabs)
library(rpart)
options(digits = 3)
data("tissue_gene_expression")


# q1
library(caret)
dat <- as.data.frame(tissue_gene_expression)

fit_rpart <- train(y ~ .,
                   method = "rpart",
                   data = dat,
                   tuneGrid = data.frame(cp = seq(0, 0.1, 0.01))
                   )
fit_rpart$bestTune


# q2
set.seed(1991, sample.kind="Rounding")    # will make R 3.6 generate a seed as in R 3.5
train_rpart <- train(y ~ .,
                     method = "rpart",
                     data = dat,
                     tuneGrid = data.frame(cp = seq(0, 0.1, 0.01)),
                     control = rpart.control(minsplit = 0)
                     )
confusionMatrix(train_rpart)


# q3
fit <- rpart(y ~ ., data = dat)
plot(fit, margin = 0.1)
text(fit, cex = 0.75)


#q4
set.seed(1991, sample.kind="Rounding")    # will make R 3.6 generate a seed as in R 3.5
fit <- train(y ~ .,
             method = "rf",
             data = dat,
             tuneGrid = data.frame(mtry = seq(50, 200, 25)),
             nodesize = 1
)
fit$bestTune
ggplot(fit, highlight = TRUE)


# q5
imp <- varImp(fit)
