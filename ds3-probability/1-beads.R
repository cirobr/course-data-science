# criar urna com 2 bolas vermelhas e 3 azuis
beads <- rep(c("red", "blue"), times = c(2, 3))

# retira 1 bolinha da urna
sample(beads, 1)                  #without replacement (n?o devolve)
#sample(beads, 6)                  #erro. max 5 bolas sem reposi??o
sample(beads, 1, replace = TRUE)  #with replacement (devolve e sorteia novamente)

# repete B vezes o mesmo evento (retira uma bolinha)
B <- 10000
events <- replicate(B, sample(beads, 1))     #sem reposi??o/eventos dependentes
head(events)

tab <- table(events)
tab
prop.table(tab)