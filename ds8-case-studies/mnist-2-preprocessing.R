# removing low variability predictors
# nearZeroVar(x)

library(matrixStats)
sds <- colSds(x)
library(caret)
qplot(sds, bins = 256, color = I("black"))

nzv <- nearZeroVar(x)
image(matrix(1:784 %in% nzv, 28, 28))

col_index <- setdiff(1:ncol(x), nzv)
length(col_index)