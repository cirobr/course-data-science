vec <- c(1,8,4,9,3,6,5,2,3)
m <- matrix(vec,3,3)

m_t <- t(m)           # transpose m
m_i <- solve(m)       # inverse of m

m
m %*% m_t             # matrix algebra multiplication

m %*% m_i             # m multiplied by its inverse = identity matrix
crossprod(m_t, m_i)   # if first term is transposed, then same result as above.

qr(m)                 # entender melhor a função qr()
