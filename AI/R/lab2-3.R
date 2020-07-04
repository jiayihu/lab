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
