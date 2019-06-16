library(ISLR)
attach(Wage)
fit = lm(wage ~ poly(age, 4), data = Wage)
summary(fit)
agelims = range(age)
age.grid = seq(from = agelims[1], to = agelims[2])
preds = predict(fit, newdata = list(age = age.grid), se = TRUE) # Calculate also standard error
se.bands = cbind(preds$fit + 2 * preds$se.fit, preds$fit - 2 * preds$se.fit)
par(mfrow = c(1, 2), mar = c(4.5, 4.5, 1, 1), oma = c(0, 0, 4, 0))
plot(age, wage, xlim = agelims, cex = .5, col = "darkgrey")
title("Degree -4 Polynomial", outer = TRUE)
lines(age.grid, preds$fit, lwd = 2, col = "blue")
matlines(age.grid, se.bands, lwd = 1, col = "blue", lty = 3)

fit.1 = lm(wage ~ poly(age, 1), data = Wage)
fit.2 = lm(wage ~ poly(age, 2), data = Wage)
fit.3 = lm(wage ~ poly(age, 3), data = Wage)
fit.4 = lm(wage ~ poly(age, 4), data = Wage)
fit.5 = lm(wage ~ poly(age, 5), data = Wage)
anova(fit.1, fit.2, fit.3, fit.4, fit.5) # Models must be nested

# Predict if an individual earns > 250k per year
fit = glm(I(wage > 250) ~ poly(age, 4), data = Wage, family = binomial) # default prediction type is "link", not "response"
preds = predict(fit, newdata = list(age = age.grid), se = TRUE)
pfit = exp(preds$fit) / (1+exp(preds$fit))
se.bands.logit = cbind(preds$fit + 2 * preds$se.fit, preds$fit - 2 * preds$se.fit)
se.bands = exp(se.bands.logit) / (1+ exp(se.bands.logit))
plot(age, I(wage > 250), xlim = agelims, type = "n", ylim = c(0, .2))
points(jitter(age), I((wage > 250) / 5), cex = 5, pch = "|", col = "darkgrey")
lines(age.grid, pfit, lwd = 2, col = "blue")
matlines(age.grid, se.bands, lwd = 1, col = "blue", lty = 3)

# Step functions
table(cut(age, 4))
fit = lm(wage ~ cut(age, 4), data = Wage)
summary(fit) # Intercept is the first category, base average salary for people under 33.5 years

# Splines
library(splines)
# Create basis functions with given knots, by default are cubic splines
fit = lm(wage ~ bs(age, knots = c(25, 40, 50)), data = Wage)
pred = predict(fit, newdata = list(age = age.grid), se = TRUE)
plot(age, wage, col = "gray")
lines(age.grid, pred$fit, lwd = 2)
lines(age.grid, pred$fit + 2 * pred$se.fit, lty = "dashed")
lines(age.grid, pred$fit - 2 * pred$se.fit, lty = "dashed")
dim(bs(age, knots = c(25, 40, 60)))
dim(bs(age, df = 6))
attr(bs(age, df = 6), "knots")

# Natural spline
fit2 = lm(wage ~ ns(age, df = 4), data = Wage)
pred2 = predict(fit2, newdata = list(age = age.grid), se = TRUE)
lines(age.grid, pred2$fit, col = "red", lwd = 2)

# Smoothing spline
plot(age, wage, xlim = agelims, cex = .5, col = "darkgrey")
title("Smoothing Spline")
fit = smooth.spline(age, wage, df = 16)
fit2 = smooth.spline(age, wage, cv = TRUE) # Find df by using LOOCV
fit2$df # 6.8 df
lines(fit, col = "red", lwd = 2)
lines(fit2, col = "blue", lwd = 2)

# Local regression
plot(age, wage, xlim = agelims, cex = .5, col = "darkgrey")
title("Local Regression")
fit = loess(wage ~ age, span = .2, data = Wage)
fit2 = loess(wage ~ age, span = .5, data = Wage)
lines(age.grid, predict(fit, data.frame(age = age.grid)), col = "red", lwd = 2)
lines(age.grid, predict(fit2, data.frame(age = age.grid)), col = "blue", lwd = 2)
legend("topright", legend = c("Span = 0.2", "Span = 0.5"), col = c("red", "blue"), lty = 1, lwd = 2, cex = 0.8)

# GAM
gam1 = lm(wage ~ ns(year, 4) + ns(age, 5) + education, data = Wage)
library(gam)
gam.m3 = gam(wage ~ s(year, 4) + s(age, 5) + education, data = Wage) # use GAM with smooth spline
par(mfrow = c(3, 3))
plot(gam.m3, se = TRUE, col = "blue")
plot.Gam(gam1, se = TRUE, col = "red")

gam.m1 = gam(wage ~ s(age, 5) + education, data = Wage)
gam.m2 = gam(wage ~ year + s(age, 5) + education, data = Wage)
anova(gam.m1, gam.m2, gam.m3, test = "F")
summary(gam.m3)
preds = predict(gam.m2, newdata = Wage)
gam.lo = gam(wage ~ s(year, df = 4) + lo(age, span = 0.7) + education, data = Wage)
plot.Gam(gam.lo, se = TRUE, col = "green")

# local regression of the interaction between year and age
gam.lo.i = gam(wage ~ lo(year, age, span = 0.7) + education, data = Wage)
library(akima)
plot(gam.lo.i)

# logistic regression in GAM
gam.lr = gam(I(wage > 250) ~ year + s(age, df = 5) + education, family = binomial, data = Wage)
par(mfrow = c(1, 3))
plot(gam.lr, se = TRUE, col = "green")
table(education, I(wage > 250))

gam.lr.s = gam(I(wage > 250) ~ year + s(age, df = 5) + education, family = binomial, data = Wage, subset = (education != "1. < HS Grad"))
plot(gam.lr.s, se = TRUE, col = "green")
