rm(list = ls())
library(ISLR)
data("Hitters")
hitters = na.omit(Hitters)
dim(hitters)
hitters$Salary = log(hitters$Salary)
attach(hitters)
hist(Salary)

# Ridge and lasso regressions
library(glmnet)
y = hitters$Salary
X = model.matrix(Salary ~ ., data = hitters)[, -1]
m.ridge = glmnet(X, y, alpha = 0)
m.ridge
names(m.ridge)
coef(m.ridge) # matrix with coefs for each variable and lambda
plot.glmnet(m.ridge, xvar = "lambda")
plot(m.ridge, label = TRUE)

set.seed(2906)
cv.ridge = cv.glmnet(X, y, alpha = 0)
plot(cv.ridge)
best.lambda = cv.ridge$lambda.min
cv.ridge$cvm[cv.ridge$lambda == best.lambda] # MSE of best lambda
min(cv.ridge$cvm) # Alternative
cv.ridge$cvm[which.min(cv.ridge$lambda)] # Alternative again

m.ridge.min = glmnet(X, y, alpha = 0, lambda = best.lambda)
m.ridge.min
par(mfrow = c(1, 2))
plot(m.ridge, xvar = "lambda")
abline(v = log(best.lambda), lty = 2) # best lambda vertical line
# Plot of deviance
plot(log(m.ridge$lambda), m.ridge$dev.ratio, type = "l", xlab = "Log lambda", ylab = "Explained deviance")
abline(v = log(best.lambda), lty = 2)
max(m.ridge$dev.ratio)

## Lasso
m.lasso = glmnet(X, y, alpha = 1)
plot(m.lasso, xvar = "lambda")
set.seed(2906)
cv.lasso = cv.glmnet(X, y, alpha = 1)
best.lambda.lasso = cv.lasso$lambda.min
min(cv.lasso$cvm)
abline(v = log(best.lambda.lasso), lty = 2)
m.lasso.min = glmnet(X, y, alpha = 1, lambda = best.lambda.lasso)
coef(m.lasso.min)
par(mfrow = c(1, 2))
plot(m.lasso, xvar = "lambda")
abline(v = log(best.lambda.lasso), lty = 2) # best lambda vertical line
plot(log(m.lasso$lambda), m.lasso$dev.ratio, type = "l", xlab = "Log lambda", ylab = "Explained deviance")
abline(v = log(best.lambda.lasso), lty = 2)
max(m.lasso$dev.ratio)
