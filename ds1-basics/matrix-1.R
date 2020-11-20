vec <- c(20, 2015, "03:46",
         30, 2015, "03:50",
         40, 2015, "04:39",
         50, 2015, "04:48",
         20, 2016, "03:22")

mat <- matrix(vec,
              nrow=5,
              ncol=3,
              byrow=TRUE)
colnames(mat) <- c("Valor", "Ano", "Tempo")
mat
# rownames()
