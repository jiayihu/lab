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
