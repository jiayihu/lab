rm(list = ls())
library(pls)
data("gasoline")
names(gasoline)
dim(gasoline$NIR)
y = gasoline$octane
X = gasoline$NIR
set.seed(222)
m.pcr = pcr(y ~ X, ncomp = 20, scale = TRUE, validation = "CV")
summary(m.pcr)
par(mfrow = c(1, 2))
validationplot(m.pcr, val.type = "MSEP", main = "Gasoline")
validationplot(m.pcr, val.type = "R2", main = "Gasoline")
par(mfrow = c(1, 2))
validationplot(m.pcr, val.type = "MSEP", main = "Gasoline", axes = FALSE)
axis(1, at = 1:20)
axis(2)
validationplot(m.pcr, val.type = "R2", main = "Gasoline", axes = FALSE)
axis(1, at = 1:20)
axis(2)
# Select best number of components using one-sigma rule
selectNcomp(m.pcr, method = "onesigma", ncomp = 20)
explvar(m.pcr) # explained variance by each single component
plot(1:20, explvar(m.pcr), ylab = "% of explained variance", xlab = "PCs", type = "l", axes = FALSE)
axis(1, at=1:20)
axis(2)

coefplot(m.pcr, ncomp = 1:5, legendpos = "bottomleft", main = "", xlab = "Variables", ylab = "Regression coefficients")
coefplot(m.pcr, ncomp = 5, main = "", xlab = "Variables", ylab = "Regression coefficients")
scoreplot(m.pcr, comps = 1:5, cex = 0.5, cex.lab = 1.4, cex.axis = 1.4, pch = 19)
plot(m.pcr, xlab = "Observed values", ylab = "Predictions", main = "Model with 5 PCsi")
abline(0, 1)

library(glmnet)
set.seed(222)
m.ridge = glmnet(X, y, alpha = 0, lambda.min = 1e-4)
cv.ridge = cv.glmnet(X, y, alpha = 0, lambda.min = 1e-4)
best.lambda = cv.ridge$lambda.min
min(cv.ridge$cvm)
MSEP(m.pcr, ncomp = 5)

m.lasso = glmnet(X, y, alpha = 1, lambda.min = 1e-4)
set.seed(222)
cv.lasso = cv.glmnet(X, y, alpha = 1, lambda.min = 1e-4)
best.lambda.lasso = cv.lasso$lambda.min
m.lasso.min = glmnet(X, y, alpha = 1, lambda = best.lambda.lasso)
min(cv.lasso$cvm)

par(mfrow = c(1, 2))
plot(m.ridge, xvar = "lambda", main = "Regressione ridge")
abline(v = log(best.lambda), lty = 2)
plot(m.lasso, xvar = "lambda", main = "lasso")
abline(v = log(best.lambda.lasso), lty = 2)

id.zero = which(coef(m.lasso.min) == 0)
length(id.zero)
length(coef(m.lasso.min)) - length(id.zero)
