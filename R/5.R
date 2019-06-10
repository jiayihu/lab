# 5

library(ISLR)
attach(Auto)
set.seed(1)
train = sample(392, 196)
m = lm(mpg ~ horsepower, data = Auto, subset = train)
mean((mpg - predict(m, Auto))[-train]^2)

m1 = lm(mpg ~ poly(horsepower, 2), data = Auto, subset = train)
mean((mpg - predict(m1, Auto))[-train]^2)

m2 = lm(mpg ~ poly(horsepower, 3), data = Auto, subset = train)
mean((mpg - predict(m2, Auto))[-train]^2)

# LOOCV
m3 = glm(mpg ~ horsepower, data = Auto)
m3_err = cv.glm(Auto, m3)
m3_err$delta

errors = rep(0, 5)
for (i in 1:5) {
  m = glm(mpg ~ poly(horsepower, i), data = Auto)
  errors[i] = cv.glm(Auto, m)$delta[1]
}
errors

# K-Fold CV
set.seed(17)
errors.10 = rep(0, 10)
for (i in 1:10) {
  m = glm(mpg ~ poly(horsepower, i), data = Auto)
  errors.10[i] = cv.glm(Auto, m, K = 10)$delta[1]
}
errors.10
