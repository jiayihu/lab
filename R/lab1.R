library(MASS)
data(Boston)
attach(Boston)
Boston[1:3,]
dim(Boston)
n = nrow(Boston)
n
summary(Boston$medv)
hist(Boston$medv, xlab = "Median value", main = "Histogram")
boxplot(Boston$medv, xlab="Median value", main = "Boxplot")
plot(Boston$lstat, Boston$medv, main = "Dispersion plot", xlab = "% of lower status of the population", ylab = "Median value", pch = 19, cex = 0.5)
cor(Boston$medv, Boston$lstat)

# mdev = beta0 + beta1 * lstat + eps
beta1 = cov(Boston$medv, Boston$lstat) / var(Boston$lstat)
beta1
beta0 = mean(Boston$medv) - beta1 * mean(Boston$lstat)
beta0
covariance = mean(medv*lstat) - mean(medv)*mean(lstat)
covariance = cov(Boston$medv, Boston$lstat) * (n-1) / n # sample covariance unbiased by having * (n-1) / n
variance = mean(Boston$lstat^2) - (mean(Boston$lstat)^2)
variance = var(Boston$lstat) * (n - 1) / n # sample variance unbiased by having * (n-1) / n

model = lm(medv ~ lstat, data = Boston)
summary(model)
est.values = fitted(model)
plot(Boston$lstat, Boston$medv, pch = 19, cex = 0.5, xlab = "% of lower status of the population", ylab = "Median value")
points(Boston$lstat, est.values, pch = "x", col = "green")
abline(coef(model)[1], coef(model)[2], lty = 2, col = "red", lwd = 3)
abline(beta0, beta1, lty = 2, col = "blue") # lty = line type dashed
res = residuals(model)
par(mfrow = c(2, 2))
hist(res, prob = TRUE)
plot(res, pch = 19, cex = 0.5, ylab = "Residuals")
abline(h=0, lty = 2)
plot(est.values, res, pch = 19, cex = 0.5, xlab = "Estimated values", ylab = "Residuals")
abline(h = 0, lty = 2)
plot(Boston$lstat, res, ylab = "Residuals", xlab = "% of lower status of the population", pch = 19, cex = 0.5)
abline(h=0, lty = 2)

par(mfrow = c(2, 2))
plot(model)

vcov(model) # variance/covariance matrix
se = sqrt(diag(vcov(model))) # standard error

# confidence interval
c(beta1 - qt(0.975, df = n - 2) * se[2], beta1 + qt(0.975, df = n - 2) * se[2])
# if n > 30
c(beta1 - qnorm(0.975) * se[2], beta1 + qnorm(0.975) * se[2])
confint(model)

# Hypothesis H0 = beta1 = -1 with significance level 0.05
statistic.t = (beta1 - (-1)) / se[2]
statistic.t # 1.289
qt(0.025, df = n - 2) # quantile for 0.025 in t-student distribution 1.96
# p-value of the test
2 * min(pt(statistic.t, n-2), 1 - pt(statistic.t, n-2))

predict(model, newdata = data.frame(list(lstat = c(5, 10, 25))))
predict(model, newdata = data.frame(list(lstat = c(5, 10, 25))), interval = "prediction")

plot(Boston$crim, Boston$medv, ylab = "Median value", xlab = "Crime", pch = 19, cex = 0.5)
model.mv = lm(medv ~ lstat + crim, data = Boston)
summary(model.mv)

model2 = lm(medv ~ poly(lstat, 2, raw = TRUE), data = Boston)
summary(model2)

rss0 = sum(model$residuals^2)
rss = (5.524^2) * (n - 2 - 1) # RSE^2 * (n - df - 1)
f = (rss0 - rss) / rss * (503) # 135.123
qf(0.95, 1, n - 3) # 3.86
1 - pf(f, 1, 503)
anova(model, model2)
par(mfrow = c(2, 2))
plot(model2)
