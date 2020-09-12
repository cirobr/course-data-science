library(tidyverse)
library(dslabs)
data("tissue_gene_expression")


# q1
library(caret)
dat <- as.data.frame(tissue_gene_expression)

set.seed(1991, sample.kind="Rounding")    # will make R 3.6 generate a seed as in R 3.5
train_rpart <- train(y ~ .,
                     method = "rpart",
                     data = dat,
                     tuneGrid = data.frame(cp = seq(0, 0.1, 0.01))
                     )
train_rpart$bestTune


# q2
set.seed(1991, sample.kind="Rounding")    # will make R 3.6 generate a seed as in R 3.5
train_rpart <- train(y ~ .,
                     method = "rpart",
                     data = dat,
                     tuneGrid = data.frame(cp = seq(0, 0.1, 0.01)),
                     control = rpart.control(minsplit = 0)
                     )
#train_rpart$bestTune
