library(MASS)
data(Boston)
Boston[1, ]
attach(Boston)
m2 = lm(medv ~ lstat + I(lstat^2), data = Boston)
summary(m2)
m3 = lm(medv ~ poly(lstat, 3), data = Boston)
summary(m3)
anova(m2, m3)
logLik(m2)
logLik(m3)
m2.glm = glm(medv ~ poly(lstat, 2), data = Boston)
m3.glm = glm(medv ~ poly(lstat, 3), data = Boston)
m2.glm$aic - m3.glm$aic

library(boot)
set.seed(123)
cv.err.m2 = cv.glm(Boston, m2.glm, K = 10)
names(cv.err.m2)
cv.err.m2$delta
cv.err.m3 = cv.glm(Boston, m3.glm, K = 10)
cv.err.m3$delta
cv.glm(Boston, m2.glm)$delta
cv.glm(Boston, m3.glm)$delta
adj.r2 = rep(0, 6)
aic = rep(0, 6)
for(i in 1:6) {
  m = lm(medv ~ poly(lstat, i), data = Boston)
  adj.r2[i] = summary(m)$adj.r.squared
  aic[i] = 2 * (i+2) - 2 * logLik(m)
}
adj.r2
aic

m5.glm = glm(medv ~ poly(lstat, 5), data = Boston)
m6.glm = glm(medv ~ poly(lstat, 6), data = Boston)

set.seed(123)
cv.err.m5 = cv.glm(Boston, m5.glm, K = 10)$delta
cv.err.m5
cv.err.m6 = cv.glm(Boston, m6.glm, K = 10)$delta
cv.err.m6
