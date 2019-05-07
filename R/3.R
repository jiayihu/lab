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
summary(lm(medv~poly(lstat, 5), data=Boston)) # Polynomial lstat
summary(lm(medv~log(lstat), data=Boston))


############
# Qualitative predictors
library(ISLR)
attach(Carseats)
lm.fit = lm(Sales~.+Income:Advertising+Price:Age, data=Carseats)
summary(lm.fit)
contrasts(ShelveLoc)
