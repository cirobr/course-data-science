options(digits = 3)
set.seed(1)
disease <- sample(c(0,1), size=1e6, replace=TRUE, prob=c(0.98,0.02))
test <- rep(NA, 1e6)
test[disease==0] <- sample(c(0,1), size=sum(disease==0), replace=TRUE, prob=c(0.90,0.10))
test[disease==1] <- sample(c(0,1), size=sum(disease==1), replace=TRUE, prob=c(0.15, 0.85))

#Q2 P(test positive)
mean(test)

#Q3 P(disease | test negative)
mean(disease[test == 0])

#Q4 P(disease | test positive)
p <- mean(disease[test == 1])
p

#Q5 P(disease | test positive) / P(disease)
p_disease <- mean(disease)
p / p_disease
