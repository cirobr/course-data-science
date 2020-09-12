library(dslabs)
data(tissue_gene_expression)

dim(tissue_gene_expression$x)
table(tissue_gene_expression$y)

x <- tissue_gene_expression$x
y <- tissue_gene_expression$y

x_select <- x[c(1, 2, 39, 40, 73, 74),]
d <- dist(x_select)
image(as.matrix(d))