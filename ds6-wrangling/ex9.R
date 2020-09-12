library(rvest)
url <- "https://web.archive.org/web/20181024132313/http://www.stevetheump.com/Payrolls.htm"
h <- read_html(url)

nodes <- html_nodes(h, "table")

html_text(nodes[[8]])

tabela <- html_table(nodes[[8]])
tabela

html_text(nodes[[1]])
html_text(nodes[[2]])
html_text(nodes[[3]])
html_text(nodes[[4]])

table2 <- html_table(nodes[[2]])
table2

table3 <- html_table(nodes[[3]])
table3

table4 <- html_table(nodes[[4]])
table4

html_table(nodes[[length(nodes) - 2]])
html_table(nodes[[length(nodes) - 1]])
html_table(nodes[[length(nodes)]])


tab_1 <- html_table(nodes[[10]])
head(tab_1)
tab_2 <- html_table(nodes[[19]])
head(tab_2)

colunas <- c("Team", "Payroll", "Average")
tab_1 <- tab_1[-1,-1]
colnames(tab_1) <- colunas
head(tab_1)

tab_2 <- tab_2[-1,]
colnames(tab_2) <- colunas
head(tab_2)
