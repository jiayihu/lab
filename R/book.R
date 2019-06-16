######### Chapter 3 #########

library(MASS)
attach(Boston)
m = lm(medv~lstat, data=Boston)
summary(m)

confint(m, level=0.95)

# Predict using confidence and prediction intervals
conf = predict(m, data.frame(lstat=c(5, 10, 15)), interval="confidence")
pred = predict(m, data.frame(lstat=c(5, 10, 15)), interval="prediction")
conf[1:3,]
pred[1:3,]

plot(lstat, medv)
abline(m, col = "red")

# Regression plots
par(mfrow = c(2, 2))
plot(m)

# Residuals
par(mfrow = c(2, 2))
plot(predict(m), residuals(m))
plot(predict(m), rstudent(m))

# Leverage values
plot(hatvalues(m))
which.max(hatvalues(m))

# Multiple linear regression
summary(lm(medv~lstat+age, data=Boston))

summary(lm(medv~., data=Boston))

summary(lm(medv~.-age, data=Boston))
cor(Boston)

# Interaction terms
summary(lm(medv~lstat*age, data=Boston))

# Non-linear transformations
m1 = lm(medv~lstat, data=Boston)
m2 = lm(medv~lstat+I(lstat^2), data=Boston)
anova(m1, m2) # Compare models and evaluate null hypothesis that both models are equally good
summary(lm(medv~poly(lstat, 5, raw = T), data=Boston)) # Polynomial lstat
summary(lm(medv~log(lstat), data=Boston))


############
# Qualitative predictors
library(ISLR)
attach(Carseats)
lm.fit = lm(Sales~.+Income:Advertising+Price:Age, data=Carseats)
summary(lm.fit)
contrasts(ShelveLoc) # shows coding used for the dummy variable

######### Chapter 4 #########

# 4

library(ISLR)
names(Smarket)
attach(Smarket)
dim(Smarket)
summary(Smarket)
cor(Smarket[, -9])
plot(Volume)
m = glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data = Smarket, family = binomial)
summary(m)
coef(m)
m_probs = predict(m, type="response") # output probabilities of P(Y=1|X)
m_probs[1:10]
contrasts(Direction) # 1 == Up, 0 == Down
m_pred = rep("Down", 1250)
m_pred[m_probs > .5] = "Up"
addmargins(table(m_pred, Direction))
mean(m_pred == Direction) # % of correct predictions
mean(m_pred != Direction) # training error rate

train = (Year < 2005)
Smarket.2005 = Smarket[!train,]
dim(Smarket.2005)
Direction.2005 = Direction[!train]
m = glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data = Smarket, family = binomial, subset = train)
m_probs = predict(m, Smarket.2005, type="response")
m_pred = rep("Down", 252)
m_pred[m_probs > .5] = "Up"
addmargins(table(m_pred, Direction.2005))
mean(m_pred == Direction.2005)
mean(m_pred != Direction.2005)

m = glm(Direction ~ Lag1+Lag2, data=Smarket, family=binomial, subset = train)
m_probs = predict(m, Smarket.2005, type="response")
m_pred = rep("Down", 252)
m_pred[m_probs > .5] = "Up"
addmargins(table(m_pred, Direction.2005))
mean(m_pred == Direction.2005)
106 / (106 + 76) # accuracy rate of increase predictions
predict(m, newdata = data.frame(Lag1 = c(1.2, 1.5), Lag2 = c(1.1, -0.8)), type="response")

## LDA
library(MASS)
m = lda(Direction ~ Lag1 + Lag2, data=Smarket, subset = train)
m
plot(m)
m_pred = predict(m, Smarket.2005)
names(m_pred)
addmargins(table(m_pred$class, Direction.2005)) # table of predictions and actual values
mean(m_pred$class == Direction.2005)
sum(m_pred$posterior[,1] >= 0.5)
sum(m_pred$posterior[,1] < 0.5)
m_pred$posterior[1:20, 1]
m_pred$class[1:20]
sum(m_pred$posterior[, 1] > 0.9)

## QDA
m = qda(Direction~Lag1+Lag2, data=Smarket, subset = train)
m
m_pred = predict(m, Smarket.2005)
table(m_pred$class, Direction.2005)
mean(m_pred$class == Direction.2005)

## KNN
library(class)
train.X = cbind(Lag1, Lag2)[train,]
test.X = cbind(Lag1, Lag2)[!train,]
train.Direction = Direction[train]
set.seed(1)
pred = knn(train.X, test.X, train.Direction, k = 1)
table(pred, Direction.2005)
mean(pred == Direction.2005)

pred = knn(train.X, test.X, train.Direction, k = 3)
table(pred, Direction.2005)
mean(pred == Direction.2005)

### Caravan
dim(Caravan)
attach(Caravan)
summary(Purchase)
standardized.X = scale(Caravan[,-86])
var(Caravan[, 1])
var(Caravan[, 2])
var(standardized.X[, 1])
var(standardized.X[, 2])
test = 1:1000
train.X = standardized.X[-test,]
test.X = standardized.X[test,]
train.Y = Purchase[-test]
test.Y = Purchase[test]
set.seed(1)
pred = knn(train.X, test.X, train.Y, k = 1)
mean(pred != test.Y)
mean(test.Y == "Yes")
pred
table(pred, test.Y)
9 / (68+9)

pred = knn(train.X, test.X, train.Y, k = 3)
table(pred, test.Y)
5/26

pred = knn(train.X, test.X, train.Y, k = 5)
table(pred, test.Y)
4/15

m = glm(Purchase ~ ., data = Caravan, family = binomial, subset = -test)
m_probs = predict(m, Caravan[test,], type="response")
m_pred = rep("No", 1000)
m_pred[m_probs > .5] = "Yes"
table(m_pred, test.Y)

m_pred = rep("No", 1000)
m_pred[m_probs > .25] = "Yes"
table(m_pred, test.Y)
11/33

######### Chapter 5 #########

# 5

library(ISLR)
attach(Auto)
set.seed(1)
train = sample(392, 196)
m = lm(mpg ~ horsepower, data = Auto, subset = train)
mean((mpg - predict(m, Auto))[-train]^2)
mean((mpg - predict(m, newdata = Auto[-train, ]))^2)

m1 = lm(mpg ~ poly(horsepower, 2), data = Auto, subset = train)
mean((mpg - predict(m1, Auto))[-train]^2)

m2 = lm(mpg ~ poly(horsepower, 3), data = Auto, subset = train)
mean((mpg - predict(m2, Auto))[-train]^2)

# LOOCV
library(boot)
m3 = glm(mpg ~ horsepower, data = Auto)
m3_err = cv.glm(Auto, m3)
m3_err$delta

errors = rep(0, 5)
for (i in 1:5) {
  m = glm(mpg ~ poly(horsepower, i), data = Auto)
  errors[i] = cv.glm(Auto, m)$delta[1]
}
errors
which.min(errors)

# K-Fold CV
set.seed(17)
errors.10 = rep(0, 10)
for (i in 1:10) {
  m = glm(mpg ~ poly(horsepower, i), data = Auto)
  errors.10[i] = cv.glm(Auto, m, K = 10)$delta[1]
}
errors.10
which.min(errors.10)

# 5.5

library(ISLR)
attach(Default)
set.seed(1)
m = glm(default ~ income + balance, data = Default, family = binomial)
train = sample(dim(Default)[1], dim(Default)[1] / 2)
m1 = glm(default ~ income + balance, data = Default, family = binomial, subset = train)
m1_probs = predict(m1, Default[-train,], type = "response")
m1_pred = rep("No", dim(Default)[1] / 2)
m1_pred[m1_probs > .5] = "Yes"
addmargins(table(m1_pred, default[-train]))
mean(m1_pred != default[-train])

m2 = glm(default ~ income + balance + student, data = Default, family = binomial, subset = train)
m2_probs = predict(m2, Default[-train,], type = "response")
m2_pred = rep("No", dim(Default)[1] / 2)
m2_pred[m2_probs > .5] = "Yes"
table(m2_pred, default[-train])
mean(m2_pred != default[-train])

######### Chapter 6 #########

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
mean.cv.errors = apply(cv.errors, 2, mean) # test MSE for each model size, apply to columns
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

######### Chapter 7 #########

library(ISLR)
attach(Wage)
fit = lm(wage ~ poly(age, 4), data = Wage)
summary(fit)
agelims = range(age)
age.grid = seq(from = agelims[1], to = agelims[2])
preds = predict(fit, newdata = list(age = age.grid), se = TRUE) # Calculate also standard error
se.bands = cbind(preds$fit + 2 * preds$se.fit, preds$fit - 2 * preds$se.fit)
par(mfrow = c(1, 2), mar = c(4.5, 4.5, 1, 1), oma = c(0, 0, 4, 0))
plot(age, wage, xlim = agelims, cex = .5, col = "darkgrey")
title("Degree -4 Polynomial", outer = TRUE)
lines(age.grid, preds$fit, lwd = 2, col = "blue")
matlines(age.grid, se.bands, lwd = 1, col = "blue", lty = 3)

fit.1 = lm(wage ~ poly(age, 1), data = Wage)
fit.2 = lm(wage ~ poly(age, 2), data = Wage)
fit.3 = lm(wage ~ poly(age, 3), data = Wage)
fit.4 = lm(wage ~ poly(age, 4), data = Wage)
fit.5 = lm(wage ~ poly(age, 5), data = Wage)
anova(fit.1, fit.2, fit.3, fit.4, fit.5) # Models must be nested

# Predict if an individual earns > 250k per year
fit = glm(I(wage > 250) ~ poly(age, 4), data = Wage, family = binomial) # default prediction type is "link", not "response"
preds = predict(fit, newdata = list(age = age.grid), se = TRUE)
pfit = exp(preds$fit) / (1+exp(preds$fit))
se.bands.logit = cbind(preds$fit + 2 * preds$se.fit, preds$fit - 2 * preds$se.fit)
se.bands = exp(se.bands.logit) / (1+ exp(se.bands.logit))
plot(age, I(wage > 250), xlim = agelims, type = "n", ylim = c(0, .2))
points(jitter(age), I((wage > 250) / 5), cex = 5, pch = "|", col = "darkgrey")
lines(age.grid, pfit, lwd = 2, col = "blue")
matlines(age.grid, se.bands, lwd = 1, col = "blue", lty = 3)

# Step functions
table(cut(age, 4))
fit = lm(wage ~ cut(age, 4), data = Wage)
summary(fit) # Intercept is the first category, base average salary for people under 33.5 years

# Splines
library(splines)
# Create basis functions with given knots, by default are cubic splines
fit = lm(wage ~ bs(age, knots = c(25, 40, 50)), data = Wage)
pred = predict(fit, newdata = list(age = age.grid), se = TRUE)
plot(age, wage, col = "gray")
lines(age.grid, pred$fit, lwd = 2)
lines(age.grid, pred$fit + 2 * pred$se.fit, lty = "dashed")
lines(age.grid, pred$fit - 2 * pred$se.fit, lty = "dashed")
dim(bs(age, knots = c(25, 40, 60)))
dim(bs(age, df = 6))
attr(bs(age, df = 6), "knots")

# Natural spline
fit2 = lm(wage ~ ns(age, df = 4), data = Wage)
pred2 = predict(fit2, newdata = list(age = age.grid), se = TRUE)
lines(age.grid, pred2$fit, col = "red", lwd = 2)

# Smoothing spline
plot(age, wage, xlim = agelims, cex = .5, col = "darkgrey")
title("Smoothing Spline")
fit = smooth.spline(age, wage, df = 16)
fit2 = smooth.spline(age, wage, cv = TRUE) # Find df by using LOOCV
fit2$df # 6.8 df
lines(fit, col = "red", lwd = 2)
lines(fit2, col = "blue", lwd = 2)

# Local regression
plot(age, wage, xlim = agelims, cex = .5, col = "darkgrey")
title("Local Regression")
fit = loess(wage ~ age, span = .2, data = Wage)
fit2 = loess(wage ~ age, span = .5, data = Wage)
lines(age.grid, predict(fit, data.frame(age = age.grid)), col = "red", lwd = 2)
lines(age.grid, predict(fit2, data.frame(age = age.grid)), col = "blue", lwd = 2)
legend("topright", legend = c("Span = 0.2", "Span = 0.5"), col = c("red", "blue"), lty = 1, lwd = 2, cex = 0.8)

# GAM
gam1 = lm(wage ~ ns(year, 4) + ns(age, 5) + education, data = Wage)
library(gam)
gam.m3 = gam(wage ~ s(year, 4) + s(age, 5) + education, data = Wage) # use GAM with smooth spline
par(mfrow = c(3, 3))
plot(gam.m3, se = TRUE, col = "blue")
plot.Gam(gam1, se = TRUE, col = "red")

gam.m1 = gam(wage ~ s(age, 5) + education, data = Wage)
gam.m2 = gam(wage ~ year + s(age, 5) + education, data = Wage)
anova(gam.m1, gam.m2, gam.m3, test = "F")
summary(gam.m3)
preds = predict(gam.m2, newdata = Wage)
gam.lo = gam(wage ~ s(year, df = 4) + lo(age, span = 0.7) + education, data = Wage)
plot.Gam(gam.lo, se = TRUE, col = "green")

# local regression of the interaction between year and age
gam.lo.i = gam(wage ~ lo(year, age, span = 0.7) + education, data = Wage)
library(akima)
plot(gam.lo.i)

# logistic regression in GAM
gam.lr = gam(I(wage > 250) ~ year + s(age, df = 5) + education, family = binomial, data = Wage)
par(mfrow = c(1, 3))
plot(gam.lr, se = TRUE, col = "green")
table(education, I(wage > 250))

gam.lr.s = gam(I(wage > 250) ~ year + s(age, df = 5) + education, family = binomial, data = Wage, subset = (education != "1. < HS Grad"))
plot(gam.lr.s, se = TRUE, col = "green")
