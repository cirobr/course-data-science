library(dslabs)
library(caret)
library(tidyverse)
data("tissue_gene_expression")

# set.seed(1993) #if using R 3.5 or earlier
set.seed(1993, sample.kind="Rounding") # if using R 3.6 or later
ind <- which(tissue_gene_expression$y %in% c("cerebellum", "hippocampus"))
y <- droplevels(tissue_gene_expression$y[ind])
x <- tissue_gene_expression$x[ind, ]
x <- x[, sample(ncol(x), 10)]


# q1
options(digits = 3)
train_lda <- train(x, y, method = "lda")
train_lda$results$Accuracy


# q2
res <- as.data.frame(train_lda$finalModel$means)
c(max(res["cerebellum",]), which.max(res["cerebellum",]))
c(max(res["hippocampus",]), which.max(res["hippocampus",]))

res <- res %>% select(-"RAB1B")
c(max(res["cerebellum",]), which.max(res["cerebellum",]))
c(max(res["hippocampus",]), which.max(res["hippocampus",]))


# q3
library(dslabs)      
library(caret)
data("tissue_gene_expression")

set.seed(1993) #set.seed(1993, sample.kind="Rounding") if using R 3.6 or later
ind <- which(tissue_gene_expression$y %in% c("cerebellum", "hippocampus"))
y <- droplevels(tissue_gene_expression$y[ind])
x <- tissue_gene_expression$x[ind, ]
x <- x[, sample(ncol(x), 10)]

train_qda <- train(x, y, method = "qda")
train_qda$results$Accuracy


# q4
res <- as.data.frame(train_qda$finalModel$means)
c(max(res["cerebellum",]), which.max(res["cerebellum",]))
c(max(res["hippocampus",]), which.max(res["hippocampus",]))

res <- res %>% select(-"RAB1B")
c(max(res["cerebellum",]), which.max(res["cerebellum",]))
c(max(res["hippocampus",]), which.max(res["hippocampus",]))


# q5
train_lda <- train(x, y, method = "lda", preProcess = "center")
train_lda$results$Accuracy
res <- as.data.frame(train_lda$finalModel$means)
res
c(max(res["cerebellum",]), which.max(res["cerebellum",]))
c(max(res["hippocampus",]), which.max(res["hippocampus",]))


# q6
library(dslabs)      
library(caret)
data("tissue_gene_expression")

# set.seed(1993) # if using R 3.5 or earlier
set.seed(1993, sample.kind="Rounding") # if using R 3.6 or later
y <- tissue_gene_expression$y
x <- tissue_gene_expression$x
x <- x[, sample(ncol(x), 10)]

train_lda <- train(x, y, method = "lda", preProcess = "center")
train_lda$results$Accuracy
