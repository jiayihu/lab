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
