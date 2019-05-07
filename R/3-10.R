library(ISLR)
attach(Carseats)
m = lm(Sales~Price+Urban+US, data=Carseats)
summary(m)

m1 = lm(Sales~Price+US, data=Carseats)
summary(m1)
confint(m1)

par(mfrow=c(2, 3))
plot(m1)
plot(rstudent(m1))
