# 5.5

library(ISLR)
attach(Default)
set.seed(1)
m = glm(default ~ income + balance, data = Default, family = binomial)
train = sample(dim(Default)[1], dim(Default)[1] / 2)
m1 = glm(default ~ income + balance, data = Default, family = binomial, subset = train)
m1_probs = predict(m1, Default[-train,], type = "response")
m1_pred = rep("No", dim(Default)[1] / 2)
m1_pred[m1_probs > .5] = "Yes"
addmargins(table(m1_pred, default[-train]))
mean(m1_pred != default[-train])

m2 = glm(default ~ income + balance + student, data = Default, family = binomial, subset = train)
m2_probs = predict(m2, Default[-train,], type = "response")
m2_pred = rep("No", dim(Default)[1] / 2)
m2_pred[m2_probs > .5] = "Yes"
table(m2_pred, default[-train])
mean(m2_pred != default[-train])

