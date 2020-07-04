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
