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
