rm(list = ls())
load("Leukemia.RData")
table(Leukemia$y)
dim(Leukemia$x)
m = glm(y ~ x, data = Leukemia, family = binomial())
leukemia.ridge = glmnet(Leukemia$x, Leukemia$y, alpha = 0, family = "binomial")
plot(leukemia.ridge, xvar = "lambda")
summary(leukemia.ridge$lambda)
set.seed(111)
cv.leukemia.ridge = cv.glmnet(Leukemia$x, Leukemia$y, alpha = 0, family = "binomial", lambda.min = 1e-4)
best.lambda.leukemia = cv.leukemia.ridge$lambda.min
best.lambda.leukemia
min(cv.leukemia.ridge$cvm)
plot(cv.leukemia.ridge)
leukemia.ridge.min = glmnet(Leukemia$x, Leukemia$y, alpha = 0, family = "binomial", lambda = best.lambda.leukemia)
leukemia.ridge = glmnet(Leukemia$x, Leukemia$y, alpha = 0, family = "binomial", lambda.min = 1e-4)
par(mfrow=c(1, 2))
plot(leukemia.ridge, xvar = "lambda")
abline(v = log(best.lambda.leukemia), lty = 2)
plot(log(leukemia.ridge$lambda), leukemia.ridge$dev.ratio, type = "l", xlab = expression(log(lambda)), ylab = "Explained deviance")
abline(v = log(best.lambda.leukemia), lty = 2)
max(leukemia.ridge$dev.ratio)

# Lasso
leukemia.lasso = glmnet(Leukemia$x, Leukemia$y, alpha = 1, family = "binomial", lambda.min = 1e-4)
plot(leukemia.lasso, xvar = "lambda")
plot(leukemia.lasso)
set.seed(111)
cv.leukemia.lasso = cv.glmnet(Leukemia$x, Leukemia$y, alpha = 1, family = "binomial", lambda.min = 1e-4)
best.lambda.leukemia.lasso = cv.leukemia.lasso$lambda.1se
best.lambda.leukemia.lasso
min(cv.leukemia.lasso$cvm)
plot(cv.leukemia.lasso)
leukemia.lasso.min = glmnet(Leukemia$x, Leukemia$y, alpha = 1, family = "binomial", lambda = best.lambda.leukemia.lasso)

# Coefficients
id.zero = which(coef(leukemia.lasso.min) == 0)
length(id.zero)
nonzero = length(coef(leukemia.lasso.min)) - length(id.zero)
nonzero # 31, but one is intercept so 30 
id.nonzero = which(coef(leukemia.lasso.min) != 0)
varnames = rownames(coef(leukemia.lasso.min))[id.nonzero]
values = coef(leukemia.lasso.min)[id.nonzero]
names(values) = varnames
values
