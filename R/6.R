# 6

library(ISLR)
View(Hitters)
dim(Hitters)
sum(is.na(Hitters$Salary))
Hitters = na.omit(Hitters)
dim(Hitters)
library(leaps)
m1 = regsubsets(Salary ~ ., data = Hitters)
summary(m1)
m2 = regsubsets(Salary ~ ., data = Hitters, nvmax = 19)
summary(m2)

m2.summary = summary(m2)
names(m2.summary)

m2.summary$rsq # R^2
m2.summary$adjr2

par(mfrow = c(2, 2))
plot(m2.summary$rss, xlab = "Number of variables", ylab = "RSS", type = "l")
plot(m2.summary$adjr2, xlab = "Number of variables", ylab = "Adjusted R^2", type = "l")
maxAdjr2 = which.max(m2.summary$adjr2) # 11
points(maxAdjr2, m2.summary$adjr2[maxAdjr2], col = "red", cex = 2, pch = 20) # Add to existing plot

plot(m2.summary$cp, xlab = "Number of variables", ylab = "Cp", type = "l")
minCp = which.min(m2.summary$cp) # 10
points(minCp, m2.summary$cp[minCp], col = "red", cex = 2, pch = 20)

plot(m2.summary$bic, xlab = "Number of variables", ylab = "BIC", type = "l")
minBIC = which.min(m2.summary$bic) # 6
points(minBIC, m2.summary$bic[minBIC], col = "red", cex = 2, pch = 20) # BIC gives heavier penalty

par(mfrow = c(2, 2))
plot(m2, scale = "r2")
plot(m2, scale = "adjr2")
plot(m2, scale = "Cp")
plot(m2, scale = "bic") # top darkest row is the model with min BIC and 6 variables
coef(m2, 6) # Coef of model with 6 variables

fwd = regsubsets(Salary ~ ., data = Hitters, nvmax = 19, method = "forward")
summary(fwd)
bwd = regsubsets(Salary ~ ., data = Hitters, nvmax = 10, method = "backward")
summary(bwd)

coef(m2, 6)
coef(fwd, 6)
coef(bwd, 6) # backward selection has CRUns instead of CRBI

## Using validation set to pick the best model, instead of Cp, BIC or adjusted R^2
set.seed(1)
train = sample(c(TRUE, FALSE), nrow(Hitters), rep = TRUE)
test = !train
best = regsubsets(Salary ~ ., data = Hitters[train, ], nvmax = 19)
test.mat = model.matrix(Salary ~ ., data = Hitters[test, ]) # transform factors to dummy variables
test.errors = rep(NA, 10)
for(i in 1:19) {
  coefi = coef(best, i)
  pred = test.mat[, names(coefi)] %*% coefi
  test.errors[i] = mean((Hitters$Salary[test] - pred)^2)
}
test.errors
which.min(test.errors)
coef(best, 10)

predict.regsubsets = function(object, newdata, id, ...) {
  form = as.formula(object$call[[2]])
  mat = model.matrix(form, newdata)
  coefi = coef(object, id = id)
  xvars = names(coefi)
  mat[, xvars] %*% coefi
}

# Trying the new function
for(i in 1:19) {
  pred = predict.regsubsets(best, Hitters[test, ], i)
  test.errors[i] = mean((Hitters$Salary[test] - pred)^2)
}
test.errors
which.min(test.errors)
coef(best, 10)

best = regsubsets(Salary ~ ., data = Hitters, nvmax = 19) # Best subset with 10 variables using the full dataset
coef(best, 10)

## Cross validation with k-folds
set.seed(1)
k = 10
folds = sample(1:k, nrow(Hitters), replace = TRUE)
cv.errors = matrix(NA, k, 19, dimnames = list(NULL, paste(1:19)))
for(j in 1:k) {
  best = regsubsets(Salary ~ ., data = Hitters[folds != j, ], nvmax = 19)
  for(i in 1:19) {
    pred = predict.regsubsets(best, Hitters[folds == j, ], i)
    cv.errors[j, i] = mean((Hitters$Salary[folds == j] - pred)^2)
  }
}
dim(cv.errors) # 10 x 19, 10 folds and 19 errors, one for each model size
mean.cv.errors = apply(cv.errors, 2, mean) # test MSE for each model size
mean.cv.errors
par(mfrow=c(1, 1))
plot(mean.cv.errors, type = 'b')
which.min(mean.cv.errors) # 11

best = regsubsets(Salary ~ ., data = Hitters, nvmax = 19) # Best subset with 11 variables using the full dataset
coef(best, 11)

## Ridge regression
library(glmnet)
# remove intercept, also glmnet doesn't convert factors to dummy variables
x = model.matrix(Salary ~ ., Hitters)[, -1]
y = Hitters$Salary
# vector of lambda, although it's not required because glmnet has a built-in range of lambda
grid = 10^seq(10, -2, length = 100)
ridge.mod = glmnet(x, y, alpha = 0, lambda = grid) # alpha = 0 for Ridge
dim(coef(ridge.mod)) # Coef for each variable (20 rows) and lambda value (100 cols)

ridge.mod$lambda[50]
coef(ridge.mod)[, 50]
sqrt(sum(coef(ridge.mod)[-1, 50]^2)) # l2 norm, 6.36

ridge.mod$lambda[60]
coef(ridge.mod)[, 60]
sqrt(sum(coef(ridge.mod)[-1, 60]^2)) # l2 norm, 57.1 much larger than before, aka more penalty

predict(ridge.mod, s = 50, type = "coefficients")[1:20,] # Ridge coeff for lambda = 50

## Estimate test error for ridge regression and lasso
set.seed(1)
train = sample(1:nrow(x), nrow(x) / 2) # random indexes
test = (-train)
y.test = y[test]

ridge.mod = glmnet(x[train, ], y[train], alpha = 0, lambda = grid, thresh = 1e-12)
ridge.pred = predict(ridge.mod, s = 4, newx = x[test,]) # predict with lambda = 4 and using test data
mean((ridge.pred - y.test)^2)

# predict with just intercept since lambda is very high and all coef are zero
ridge.pred = predict(ridge.mod, s = 1e10, newx = x[test,])
mean((ridge.pred - y.test)^2)

# Perform as least squares regressions, that it predict with lambda = 0
ridge.pred = predict(ridge.mod, s = 0, newx = x[test,], exact = TRUE)
mean((ridge.pred - y.test)^2)
lm(y ~ x, subset = train)
predict(ridge.mod, s = 0, exact = TRUE, x = x[train, ], y = y[train], type = "coefficients")[1:20,]

## Use cross-validation to pick the best lambda
set.seed(1)
cv.out = cv.glmnet(x[train, ], y[train], alpha = 0)
plot(cv.out)
bestlam = cv.out$lambda.min
bestlam
ridge.pred = predict(ridge.mod, s = bestlam, newx = x[test,])
mean((ridge.pred - y.test)^2) # test MSE is lower than before with lambda = 4

# Refit ridge with full dataset and best lambda
out = glmnet(x, y, alpha = 0)
plot(out, xvar = "lambda")
predict(out, s = bestlam, type = "coefficients")[1:20,] # No coefficient is zero

lasso.mod = glmnet(x[train, ], y[train], alpha = 1, lambda = grid)
plot(lasso.mod)
set.seed(1)
cv.out = cv.glmnet(x[train, ], y[train], alpha = 1)
plot(cv.out)
bestlam = cv.out$lambda.min
lasso.pred = predict(lasso.mod, s = bestlam, newx = x[test, ])
mean((lasso.pred - y.test)^2)
out = glmnet(x, y, alpha = 1, lambda = grid)
lasso.coef = predict(out, s = bestlam, type = "coefficients")[1:20,]
lasso.coef
lasso.coef[lasso.coef != 0]

## PCR and PLS
library(pls)
set.seed(2)
# standardize predictors and use 10-k fold CV
pcr.fit = pcr(Salary ~ ., data = Hitters, scale = TRUE, validation = "CV")
summary(pcr.fit) # CV is root, MSE = CV ^ 2
validationplot(pcr.fit, val.type = "MSEP") # Plot CV MSE
names(pcr.fit)

set.seed(1)
pcr.fit = pcr(Salary ~ ., data = Hitters, subset = train, scale = TRUE, validation = "CV")
validationplot(pcr.fit, val.type = "MSEP")
pcr.pred = predict(pcr.fit, x[test, ], ncomp = 7) # M = 7 has lowest CV MSE
mean((pcr.pred - y.test)^2)
pcr.fit = pcr(y ~ x, scale = TRUE, ncomp = 7)
summary(pcr.fit)

# PLS
set.seed(1)
pls.fit = plsr(Salary ~ ., data = Hitters, subset = train, scale = TRUE, validation = "CV")
summary(pls.fit)
validationplot(pls.fit, val.type = "MSEP") # Black line is CV, red line is adjCV
pls.pred = predict(pls.fit, x[test, ], ncomp = 2)
mean((pls.pred - y.test)^2)
pls.fit = plsr(Salary ~ ., data = Hitters, scale = TRUE, ncomp = 2)
summary(pls.fit)
