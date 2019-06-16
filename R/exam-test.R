library(ISLR)
Data = Cars93[, c("Price", "MPG.city", "Horsepower", "Origin", "AirBags")]
View(Data)
attach(Data)
hist(Price)
hist(log(Price))
Data$Price = log(Data$Price)
Price = log(Price)
boxplot(Price)
pairs(Data[, 1:3])
boxplot(Price ~ Origin)
boxplot(Price ~ AirBags)
plot(Price ~ MPG.city, col = AirBags)
contrasts(AirBags)

cor(cbind(Price, MPG.city, Horsepower, as.factor(Origin), as.factor(AirBags)))

m2 = lm(Price ~ MPG.city, data = Data)
summary(m2)

m3 = lm(Price ~ MPG.city + Horsepower, data = Data)
summary(m3)
anova(m2, m3)

m4 = lm(Price ~ MPG.city + Horsepower + Origin, data = Data)
summary(m4)
anova(m3, m4)

m1 = lm(Price ~ ., data = Data)
summary(m1)
par(mfrow = c(2, 2))
plot(m1)

summary(Data)
Data[39, ] # High leverage value and outlier

Data1 = Data[-39, ]
m5 = lm(Price ~ ., data = Data1)
summary(m5) # Let's keep the value

library(boot)
set.seed(123)
glm1 = glm(Price ~ MPG.city + Horsepower  + Origin, data = Data)
cv.glm1 = cv.glm(Data, glm1)
cv.glm1$delta

set.seed(123)
glm2 = glm(Price ~ MPG.city + Horsepower + Origin + AirBags, data = Data)
cv.glm2 = cv.glm(Data, glm2)
cv.glm2$delta

m6 = lm(Price ~ poly(MPG.city, 2, raw = TRUE) + Horsepower + Origin + AirBags, data = Data)
summary(m6)
summary(m1)
anova(m1, m6)

m7 = lm(Price ~ poly(MPG.city, 2, raw = TRUE) + poly(Horsepower, 2, raw = TRUE) + Origin + AirBags, data = Data)
summary(m7)
anova(m6, m7) # Keep m6

par(mfrow = c(2, 2))
plot(m6)

sp.mpg = smooth.spline(x = MPG.city, y = Price, cv = TRUE)
sp.mpg

sp.hp = smooth.spline(x = Horsepower, y = Price, cv = TRUE)
sp.hp

library(gam)
m.gam = gam(Price ~ s(MPG.city, sp.mpg$df) + s(Horsepower, sp.hp$df) + Origin + AirBags, data = Data)
summary(m.gam)

m.gam2 = gam(Price ~ s(MPG.city, 4) + Horsepower + Origin + AirBags, data = Data)
summary(m.gam2)

anova(m.gam, m.gam2)

par(mfrow = c(1, 4))
plot(m.gam2, se = TRUE)
par(mfrow = c(1, 2))
plot(Price, fitted(m6))
abline(0, 1, col = "red")
plot(Price, fitted(m.gam2))
abline(0, 1, col = "red")

library(glmnet)
rm(list = ls())
load("Cars.RData")
m.lm = lm(cars$Price ~ ., data = cars)
X = model.matrix(m.lm)[, -1]
y = cars$Price
m.ridge = glmnet(X, y, alpha = 0)
plot(m.ridge, xvar = "lambda")

set.seed(2906)
m.ridge.cv = cv.glmnet(X, y, alpha = 0)
plot(m.ridge.cv)
m.ridge.min = glmnet(X, y, alpha = 0, lambda = m.ridge.cv$lambda.min)
cbind(coef(m.lm), coef(m.ridge.min))

m.lasso = glmnet(X, y, alpha = 1)
plot(m.lasso, xvar = "lambda")
set.seed(2906)
m.lasso.cv = cv.glmnet(X, y, alpha = 1)
plot(m.lasso.cv)
m.lasso.min = glmnet(X, y, alpha = 1, lambda = m.lasso.cv$lambda.1se)
cbind(coef(m.lm), coef(m.ridge.min), coef(m.lasso.min))

par(mfrow = c(1, 2))
plot(predict(m.ridge.min, newx = X), y)
abline(0, 1, col = "red")
plot(predict(m.lasso.min, newx = X), y)
abline(0, 1, col = "red")

min(m.ridge.cv$cvm)
min(m.lasso.cv$cvm)

id.nonzero = which(coef(m.lasso.min) != 0)
varnames = rownames(coef(m.lasso.min))[id.nonzero]
values = coef(m.lasso.min)[id.nonzero]
names(values) = varnames
values

library(leaps)
m.forward = regsubsets(cars$Price ~ ., data = cars, nvmax = 22, method = "seqrep")
plot(m.forward)
which.min(summary(m.forward)$bic)

coef(m.forward, 8)
values