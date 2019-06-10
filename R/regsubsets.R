library(MASS)
library(boot)
library(ISLR)
library(leaps)

m1 = lm(medv~poly(lstat, 2, raw = TRUE), data = Boston)
summary(m1)
m2 = lm(medv~poly(lstat, 3, raw = TRUE), data = Boston)
summary(m2)
anova(m1, m2)

m1_aic = 2 * 2 - 2 * logLik(m1)
m2_aic = 2 * 3 - 2 * logLik(m2)

m1_bic = 2 * log(nrow(Boston)) - 2 * logLik(m1)
m2_bic = 3 * log(nrow(Boston)) - 2 * logLik(m2)

m1_glm = glm(medv ~ poly(lstat, 2, raw = TRUE), data = Boston) # glm con famiglia gaussiana = lm
m2_glm = glm(medv ~ poly(lstat, 3, raw = TRUE), data = Boston)

cv_m1 = cv.glm(Boston, m1_glm, K = 10)

# Hitters
boxplot(Hitters$Salary)
boxplot(log(Hitters$Salary)) # Rendiamo la distribuzione simmetrica
Hitters$Salary = log(Hitters$Salary)
Boston = na.omit(Boston)
Hitters = na.omit(Hitters)
m_forward = regsubsets(Salary ~ ., data = Hitters, method = "forward", nvmax = 19)
names(summary(m_forward))
summary(m_forward)$rss
summary(m_forward)$adjr2
summary(m_forward)$bic
which.min(summary(m_forward)$bic)
coef(m_forward, 4)

plot(m_forward)
plot(summary(m_forward)$rss, type = "l")
points(4, summary(m_forward)$rss[4], pch = 19, col = "red")

plot(summary(m_forward)$bic, type = "l")
points(4, summary(m_forward)$bic[4], pch = 19, col = "red")

plot(summary(m_forward)$adjr2, type = "l")
points(4, summary(m_forward)$adjr2[4], pch = 19, col = "red")

m_back = regsubsets(Salary ~ ., data = Hitters, method = "backward", nvmax = 19)
summary(m_back)
fit4 = lm(Salary ~ Hits + Years + CRuns + PutOuts, data = Hitters)
summary(fit4)
par(mfrow=c(2, 2))
plot(fit4)
plot(rstudent(fit4))
