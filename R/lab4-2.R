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
