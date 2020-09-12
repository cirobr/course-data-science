df1 <- data.frame(x = c("a", "b"),
                  y = c("a", "a"))

df2 <- data.frame(x = c("a", "a"),
                  y = c("a", "b")
                  )

union(df1, df2)
setdiff(df1, df2)
