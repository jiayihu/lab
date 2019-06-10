# 5.9

library(MASS)
summary(Boston) # Mean of medv
attach(Boston)
mean(medv)
standard_error = sd(medv) / sqrt(length(medv))
standard_error
