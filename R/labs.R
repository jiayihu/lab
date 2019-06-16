######### LAB 1 #########

library(MASS)
data(Boston)
attach(Boston)
Boston[1:3,]
dim(Boston)
n = nrow(Boston)
n
summary(Boston$medv)
Boston$medv = log(Boston$medv)
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
qt(0.975, df = n - 2) # quantile for 0.975 in t-student distribution 1.96
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

######### LAB 2 #########

ls()
rm(list = ls())
Data = read.csv("Gender_Discrimination.csv", sep = ",")
attach(Data)
View(Data)
dim(Data)
summary(Data)
table(Data$Gender)
boxplot(Data$Salary, las = 2, col = "grey", main = "Annual Salary") # las = 2 horizontal labels
hist(Data$Salary, breaks = 10)
pie(table(Data$Gender), labels = c("Female", "Male"))

# Distribution of salary by gender
boxplot(Salary ~ Gender, main = "Salary given gender", col = c("pink", "blue"), las = 2, ylab = "Salary", cex.axis = 0.7)
boxplot(Experience ~ Gender, main = "Experience given Gender", col = c("pink", "blue"))
plot(Experience, Salary, main = "Salary vs experience")
points(Experience[Gender == "Female"], Salary[Gender == "Female"], col = "pink", pch = 19, cex = 0.5)
points(Experience[Gender == "Male"], Salary[Gender == "Male"], col = "blue", pch = 19, cex = 0.5)
legend("topleft", pch = c(19, 19), c("Female", "Male"), col = c("pink", "blue"), bty = "n")

model = lm(Salary ~ Gender + Experience, data = Data)
summary(model)
abline(coef(model)[1], coef(model)[3], col = "pink")
abline(coef(model)[1] + coef(model)[2], coef(model)[3], col = "blue")

model2 = lm(Salary ~ Gender * Experience, data = Data)
summary(model2)
anova(model, model2)
model3 = lm(Salary ~ Gender * Experience + I(Experience^2))
summary(model3)
par(mfrow = c(2, 2))
plot(model2)

predict(model2, newdata = data.frame(list(Experience = 20, Gender = "Male")))
predict(model2, newdata = data.frame(list(Experience = 20, Gender = "Female")))

### LAB 2.2 ####

rm(list = ls())
Cement = read.table("hald.dat")
View(Cement)
colnames(Cement) = c("heat", "cal_alu", "tric_sil", "tric_fer", "dic_sil")
attach(Cement)
boxplot(Cement$heat)
pairs(Cement)
plot(dic_sil, heat)

model = lm(heat ~ cal_alu, data = Cement)
summary(model)
model2 = lm(heat ~ cal_alu + tric_sil, data = Cement)
summary(model2)
model3 = lm(heat ~ cal_alu + tric_sil + tric_fer + dic_sil, data = Cement)
summary(model3)
cor(Cement)
standard.resid = rstandard(model2)
pred = fitted(model2)
par(mfrow = c(2, 2))
plot(standard.resid)
abline(h=0, lty = 2)
plot(pred, standard.resid, xlab = "Predictions")
abline(h=0, lty = 2)
plot(cal_alu, standard.res)
abline(h=0, lty = 2)
plot(tric_sil, standard.res)
abline(h=0, lty = 2)

### LAB 2.3 ####

library(ISLR)
data("Carseats")
dim(Carseats)
attach(Carseats)

boxplot(Sales)
plot(Price, Sales)
boxplot(Sales ~ Urban)
boxplot(Sales ~ US)
boxplot(Sales ~ ShelveLoc)

# Dispersion plot according to levels of Urban
plot(Price, Sales)
points(Price[Urban == "Yes"], Sales[Urban == "Yes"], col = 2)
points(Price[Urban == "No"], Sales[Urban == "No"], col = 3)
legend("bottomleft", col = c("red", "green"), pch = c(19, 19), legend = c("Urban = Yes", "Urban = No"))

plot(Price, Sales)
points(Price[ShelveLoc == "Bad"], Sales[ShelveLoc == "Bad"], col = 2)
points(Price[ShelveLoc == "Good"], Sales[ShelveLoc == "Good"], col = 3)
points(Price[ShelveLoc == "Medium"], Sales[ShelveLoc == "Medium"], col = 4)

model = lm(Sales ~ Price + Urban + US + ShelveLoc, data = Carseats)
summary(model)

model2 = lm(Sales ~ Price + US + ShelveLoc, data = Carseats)
summary(model2)

anova(model2, model)

par(mfrow = c(2, 2))
plot(model2)
confint(model2, level = 0.95)

model3 = lm(Sales ~ Price * ShelveLoc + US, data = Carseats)
summary(model3)

# Change baseline level of ShelveLoc from Bad to Good
new.shelveloc2 = relevel(ShelveLoc, ref = "Good")

######### LAB 3 #########

rm(list = ls())
data("mtcars")
names(mtcars)
dim(mtcars)
attach(mtcars)
is.factor(am)

mtcars$am = as.factor(am)
attach(mtcars)

boxplot(mpg ~ vs, xlab = "vs", ylab = "mpg")
boxplot(mpg ~ vs * am, ylab = "mpg", xlab = "vs & am")

model = glm(vs ~ mpg * am, data = mtcars, family = binomial)
summary(model)

model2 = glm(vs ~ mpg + am, family = binomial, data = mtcars)
summary(model2)

coefs = coef(model2)
se = sqrt(diag(vcov(model2)))
c(coefs[2] - qnorm((1 + 0.9) / 2) * se[2], coefs[2] + qnorm((1 + 0.9) / 2) * se[2])
confint(model2, level = 0.9)

# accuracy of model on the basis of the deviance
1-pchisq(20.646, 29)

model3 = glm(vs ~ mpg, data = mtcars, family = binomial())
summary(model3)
anova(model3, model2, test = "Chisq")
1 - pchisq(25.533 - 20.646, 1) # p-value of the deviance difference

est.values = predict(model2)
head(est.values)
est.probs = predict(model2, type = "response") # or model2$fitted.values
head(est.probs)

plot(mpg, vs, col = am, xlab = "mpg", ylab = "vs")
curve(predict(model2, newdata = data.frame(mpg = x, am = "0"), type = "response"), add = TRUE)
curve(predict(model2, newdata = data.frame(mpg = x, am = "1"), type = "response"), add = TRUE, lty = 2, col = 2)

preds = rep(0, nrow(mtcars))
preds[est.probs > 0.5] = 1
preds
addmargins(table(preds, vs))
7 / 32 # error rate
mean(preds != vs) # error rate 

n = nrow(mtcars)
set.seed(222)
selection = sample(n, 0.6 * n, replace = FALSE)
selection
training.set = mtcars[selection,]
test.set = mtcars[-selection,]
model.train = glm(vs ~ mpg + am, data = training.set, family = binomial())
summary(model.train)
probs.test = predict(model.train, newdata = test.set, type = "response")
preds.test = rep(0, length(probs.test))
preds.test[probs.test > 0.5] = 1
addmargins(table(preds.test, test.set$vs))
mean(preds.test != test.set$vs)

## LDA
library(MASS)
model = lda(vs ~ mpg + am, data = training.set)
model
plot(model)
probs.lda = predict(model, newdata = test.set)
preds.lda = rep(0, nrow(test.set))
preds.lda[probs.lda$posterior[, 2] > 0.5] = 1
addmargins(table(preds.lda, test.set$vs))
mean(preds.lda != test.set$vs)

preds.lda2 = rep(0, nrow(test.set))
preds.lda2[probs.lda$posterior[, 2] > 0.2] = 1
addmargins(table(preds.lda2, test.set$vs))
mean(preds.lda2 != test.set$vs)
library(pROC)
values.roc = roc(test.set$vs, probs.lda$posterior[, 2])
values.roc
names(values.roc)
values.roc$sensitivities
values.roc$specificities
plot(values.roc, print.auc = TRUE)

model.qda = qda(vs ~ mpg + am, data = training.set)
model.qda
probs.qda = predict(model.qda, test.set)
probs.qda
addmargins(table(probs.qda$class, test.set$vs))
mean(probs.qda$class != test.set$vs)
values.roc = roc(test.set$vs, probs.qda$posterior[, 2])
values.roc

### LAB 3.2 ####

rm(list = ls())
library(ISLR)
data(Auto)
dim(Auto)
median.mpg = median(Auto$mpg)
new.mpg = rep(1, length(Auto$mpg))
new.mpg[Auto$mpg < median.mpg] = 0
new.auto = data.frame(new.mpg = new.mpg, Auto[, c("displacement", "horsepower", "origin")])
is.factor(new.auto$origin)
new.auto$origin = as.factor(new.auto$origin)
# change the names of the levels of the factor
levels(new.auto$origin) = c("America", "Europe", "Japan")

attach(new.auto)
par(mfrow = c(2, 2))
boxplot(displacement ~ new.mpg, data = new.auto, subset = origin == "America")
boxplot(displacement ~ new.mpg, data = new.auto, subset = origin == "Europe")
boxplot(displacement ~ new.mpg, data = new.auto, subset = origin == "Japan")
boxplot(horsepower ~ new.mpg, data = new.auto, subset = origin == "America")
boxplot(horsepower ~ new.mpg, data = new.auto, subset = origin == "Europe")
boxplot(horsepower ~ new.mpg, data = new.auto, subset = origin == "Japan")
mosaicplot(table(origin, new.mpg), las = 1)
table(origin, new.mpg)

m.auto = glm(new.mpg ~ displacement * origin + horsepower * origin, data = new.auto, family = binomial())
summary(m.auto)

1 - pchisq(195.72, 383)
m.auto2 = glm(new.mpg ~ displacement + origin + horsepower, data = new.auto, family = binomial())
summary(m.auto2)
anova(m.auto2, m.auto, test = "Chisq")
est.values = predict(m.auto)
est.probs = predict(m.auto, type = "response")
preds = rep(0, nrow(new.auto))
preds[est.probs > .5] = 1
addmargins(table(preds, new.mpg))
mean(preds != new.mpg)

### LAB 3.3 ####

rm(list = ls())
library(rattle.data)
attach(wine)
dim(wine)
barplot(table(Type))
pie(table(Type))
pairs(wine[, 2:7])
pairs(wine[, 8:13])
library(MASS)
wine.lda = lda(Type ~ ., data = wine)
wine.lda
plot(wine.lda)
wine.previsioni = predict(wine.lda)
ldahist(data = wine.previsioni$x[, 1], g = wine$Type)
ldahist(data = wine.previsioni$x[, 2], g = wine$Type)

######### LAB 4 #########

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

### LAB 4.2 ####

library(ISLR)
data(Hitters)
dim(Hitters)
sum(is.na(Hitters))
hitters = na.omit(Hitters)
sum(is.na(hitters))
par(mfrow=c(1, 2))
boxplot(hitters$Salary)
boxplot(log(hitters$Salary))
hitters$Salary = log(hitters$Salary)
library(leaps)
m.forward = regsubsets(Salary ~ ., data = hitters, nvmax = 19, method = "forward")
summary(m.forward)
names(summary(m.forward))
which.min(summary(m.forward)$rss) # 19
coef(m.forward, 19)
which.min(summary(m.forward)$bic) # 4
coef(m.forward, 4)
vcov(m.forward, 4)
sqrt(diag(vcov(m.forward, 4)))
plot(m.forward)
plot(m.forward, scale = "adjr2")

par(mfrow = c(2, 2))
plot(summary(m.forward)$rsq, type = "l")
max.rsq = which.max(summary(m.forward)$rsq)
points(max.rsq, summary(m.forward)$rsq[max.rsq], col = 2, pch = 16)

plot(summary(m.forward)$rss, type = "l")
min.rss = which.min(summary(m.forward)$rss)
points(min.rss, summary(m.forward)$rss[min.rss], col = 2, pch = 16)

plot(summary(m.forward)$adjr2, type = "l")
max.adjr2 = which.max(summary(m.forward)$adjr2)
points(max.adjr2, summary(m.forward)$adjr2[max.adjr2], col = 2, pch = 16)

plot(summary(m.forward)$bic, type = "l")
min.bic = which.min(summary(m.forward)$bic)
points(min.bic, summary(m.forward)$bic[min.bic], col = 2, pch = 16)

min.bic
coef(m.forward, 4)
model.bic = lm(Salary ~ hitters$Hits + hitters$Years + hitters$CRuns + hitters$PutOuts, data = hitters)
summary(model.bic)

par(mfrow=c(2,2))
plot(model.bic, pch=16, cex=0.7)
m.backward <- regsubsets(Salary ~ ., data=hitters, nvmax=19, method='backward')
plot(m.backward)

# Mixed selection
m.seqrep <- regsubsets(Salary ~ ., data=hitters, nvmax=19, method='seqrep')
plot(m.seqrep)

######### LAB 5 #########

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
plot(m.ridge)

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

### LAB 5.2 ####

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

######### LAB 6 #########

rm(list = ls())
library(rattle.data)
dim(wine)
attach(wine)
pairs(wine, col = Type, upper.panel = NULL, pch = 16, cex = 0.5)
legend("topright", bty = "n", legend = c("tipo 1", "tipo 2", "tipo 3"), pch = 16, col = c("black", "red", "green"))
pr = prcomp(wine[, -1], scale = TRUE)
names(pr)
pr
biplot(pr, scale = 0, cex = 0.5)
plot(pr$x[, 1:2], col = Type)
legend('bottomleft', pch=c(1,1,1), col=c(1,2,3), legend=c('Type 1', 'Type 2', 'Type 3'), bty='n')
variance = pr$sdev^2
prop.variance = variance / sum(variance)
prop.variance
par(mfrow = c(1, 2))
plot(prop.variance, xlab = "PCs", ylab = "Proportion of explained variance", type = "l", cex.lab = 0.7)
plot(cumsum(prop.variance), clab = "PCs", ylab = "Cumulative proportion of explained variance", type = "l", cex.lab = 0.7)
points(2, cumsum(prop.variance)[2], col = 2, pch = 16)

### LAB 6.2 ####

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

######### LAB 7 #########

rm(list = ls())
dim(cars)
attach(cars)
plot(speed, dist)
m.lm = lm(dist ~ speed, data = cars)
summary(m.lm)
m.poly = lm(dist ~ poly(speed, 2), data = cars)
summary(m.poly)
library(splines)
m.ns = lm(dist ~ ns(speed, 3), data = cars)
summary(m.ns)
fit.sp.cv = smooth.spline(x = speed, y = dist, cv = TRUE) # LOOCV by default
names(fit.sp.cv)
fit.sp.cv$df
fit.sp = smooth.spline(speed, dist, df = fit.sp.cv$df)
fit.sp
new.speed = seq(min(speed), max(speed), length.out = 100)
plot(speed, dist)
lines(new.speed, predict(m.lm, newdata = data.frame(speed = new.speed)), col = "orange", lty = 1, lwd = 2)
lines(new.speed, predict(m.poly, newdata = data.frame(speed = new.speed)), col = "red", lty = 2, lwd = 2)
lines(new.speed, predict(m.ns, newdata = data.frame(speed = new.speed)), col = "green", lty = 3, lwd = 2)
lines(fit.sp, col = "blue", lty = 4, lwd = 2)
legend("topleft", col = c("orange", "red", "green", "blue"), lty = 1:4, legend = c("lm", "poly", "natural spline", "smoothing spline"))
n = NROW(cars)
id.test = sample(n, n * 0.1)
cars.train = cars[-id.test, ]
cars.test = cars[id.test, ]
K = 1:20
rss = rep(0.0, length(K))
for (i in 1:length(K)) {
  m.ns.k = lm(dist ~ ns(speed, K[i]), data = cars.train)
  pred = predict(m.ns.k, newdata = data.frame(speed = cars.test$speed))
  rss[i] = sum((cars.test$dist - pred)^2)
}
id = which.min(rss) # 10
k.min = K[id]
m.ns.min = lm(dist ~ ns(speed, k.min), data = cars)
lines(new.speed, predict(m.ns.min, newdata = data.frame(speed = new.speed)), col = "black", lty = 2, lwd = 2)

### LAB 7.2 ####

rm(list = ls())
library(ISLR)
dim(College)
par(mfrow = c(1, 2))
hist(Apps, main = "Original scale")
hist(log(Apps), main = "Log transformation")
College$Apps = log(College$Apps)
college = College[, c("Apps", "Private", "PhD", "S.F.Ratio", "Accept")]
summary(college)
attach(college)
boxplot(Apps ~ Private)
pairs(college[, -2], pch = ".")
m = lm(Apps ~ Private + PhD + S.F.Ratio + poly(Accept, 2), data = college)
summary(m)
library(splines)
phd.ns2 = lm(Apps ~ ns(PhD, 2), data = college)
phd.ns3 = lm(Apps ~ ns(PhD, 3), data = college)
phd.ns4 = lm(Apps ~ ns(PhD, 4), data = college)
extractAIC(phd.ns2)
extractAIC(phd.ns3)
extractAIC(phd.ns4)

plot(college$PhD, college$Apps, xlab = "PhD", ylab = "Log(Apps)", pch = ".")
new.PhD = seq(min(college$PhD), max(college$PhD), length.out = 100)
lines(new.PhD, predict(phd.ns2, newdata = data.frame(PhD = new.PhD)), col = "red", lty = 2, lwd = 2)
lines(new.PhD, predict(phd.ns3, newdata = data.frame(PhD = new.PhD)), col = "green", lty = 2, lwd = 2)
lines(new.PhD, predict(phd.ns4, newdata = data.frame(PhD = new.PhD)), col = "blue", lty = 2, lwd = 2)
legend("topleft", legend = c("ns2", "ns3", "ns4"), col = c("red", "green", "blue"), lty = c(2, 2, 2), bty = "n")

sf.ns2 = lm(Apps ~ ns(S.F.Ratio, 2), data = college)
sf.ns3 = lm(Apps ~ ns(S.F.Ratio, 3), data = college)
sf.ns4 = lm(Apps ~ ns(S.F.Ratio, 4), data = college)
extractAIC(sf.ns2)
extractAIC(sf.ns3)
extractAIC(sf.ns4)

plot(college$S.F.Ratio, college$Apps, xlab = "S.F.Ratio", ylab = "Log(Apps)", pch = ".")
new.sfratio = seq(min(college$S.F.Ratio), max(college$S.F.Ratio), length.out = 100)
lines(new.sfratio, predict(sf.ns2, newdata = data.frame(S.F.Ratio = new.sfratio)), col = "red", lty = 2, lwd = 2)
lines(new.sfratio, predict(sf.ns3, newdata = data.frame(S.F.Ratio = new.sfratio)), col = "green", lty = 2, lwd = 2)
lines(new.sfratio, predict(sf.ns4, newdata = data.frame(S.F.Ratio = new.sfratio)), col = "blue", lty = 2, lwd = 2)
legend("topleft", legend = c("ns2", "ns3", "ns4"), col = c("red", "green", "blue"), lty = c(2, 2, 2), bty = "n")

m.ns = lm(Apps ~ Private + ns(PhD, 3) + ns(S.F.Ratio, 3) + poly(Accept, 2), data = college)
summary(m.ns)

m.ns2 = lm(Apps ~ ns(PhD, 3) + ns(S.F.Ratio, 3) + poly(Accept, 2), data = college)
summary(m.ns2)
anova(m.ns, m.ns2)

m.ns3 = lm(Apps ~ ns(PhD, 3) + ns(S.F.Ratio, 2) + poly(Accept, 2), data = college)
anova(m.ns2, m.ns3)

par(mfrow=c(2, 2))
plot(m.ns2)

accept.ns3 = lm(Apps ~ ns(Accept, 3), data = college)
accept.ns4 = lm(Apps ~ ns(Accept, 4), data = college)
accept.ns5 = lm(Apps ~ ns(Accept, 5), data = college)
extractAIC(accept.ns3)
extractAIC(accept.ns4)
extractAIC(accept.ns5)
m.ns4 = lm(Apps ~ ns(PhD, 3) + ns(S.F.Ratio, 2) + ns(Accept, 5), data = college)
summary(m.ns4)
anova(m.ns2, m.ns4)
par(mfrow = c(2, 2))
plot(m.ns4)
plot(college$Apps, predict(m.ns4)) # Good predictions are close to bisect line
abline(0, 1, col = "red", lwd = 2)
mean((college$Apps - predict(m.ns4))^2)

phd.cv = smooth.spline(x = college$PhD, y = college$Apps, cv = TRUE)
phd.fit = smooth.spline(x = college$PhD, y = college$Apps, df = phd.cv$df)
set.seed(111)
sf.cv = smooth.spline(x = college$S.F.Ratio, y = college$Apps, cv = TRUE)
sf.cv
sf.fit = smooth.spline(x = college$S.F.Ratio, y = college$Apps, df = sf.cv$df)
library(gam)
m.gam = gam(Apps ~ Private + s(PhD, 5) + s(S.F.Ratio, 6) + poly(Accept, 2), data = college)
summary(m.gam)

accept.cv = smooth.spline(x = college$Accept, y = college$Apps, cv = TRUE)
accept.fit = smooth.spline(x = college$Accept, y = college$Apps, df = accept.cv$df)
accept.fit

m.gam2 = gam(Apps ~ Private + s(PhD, 5) + s(S.F.Ratio, 6) + s(Accept, 21), data = college)
summary(m.gam2)
anova(m.gam, m.gam2)

m.gam3 = gam(Apps ~ Private + s(PhD, 5) + s(S.F.Ratio, 5) + s(Accept, 21), data = college)
summary(m.gam3)

par(mfrow = c(1, 4))
plot(m.gam3, se = TRUE)

par(mfrow = c(1, 2))
plot(college$Apps, predict(m.ns4), xlab = "log(Apps", ylab = "Predictions", pch = ".", main = "Natural splines", ylim=c(5, 11))
abline(0, 1, col = "red", lwd = 2)
plot(college$Apps, predict(m.gam3), xlab = "log(Apps", ylab = "Predictions", pch = ".", main = "Smoothing splines", ylim=c(5, 11))
abline(0, 1, col = "red", lwd = 2)

HighApps = College$Apps > 8
table(HighApps)

glm.gam = gam(HighApps ~ Private + s(PhD, 5) + s(S.F.Ratio, 6) + s(Accept, 21), data = college, family = "binomial")
summary(glm.gam)

glm.gam2 = gam(HighApps ~ Private + s(PhD, 5) + s(S.F.Ratio, 5) + s(Accept, 21), data = college, family = "binomial")
summary(glm.gam2)

glm.gam3 = gam(HighApps ~ Private + s(PhD, 5) + s(S.F.Ratio, 4) + s(Accept, 21), data = college, family = "binomial")
summary(glm.gam3)

anova(glm.gam, glm.gam2)
anova(glm.gam3, glm.gam2)
