# 5.8
library(boot)
set.seed(1)
x = rnorm(100)
y = x - 2 * x^2 + rnorm(100)
plot(x, y)

Data = data.frame(x, y)
set.seed(1)
m = glm(y ~ x)
m_err = cv.glm(Data, m)
m_err$delta

m1 = glm(y ~ poly(x, 2))
m1_err = cv.glm(Data, m1)
m1_err$delta

m2 = glm(y ~ poly(x, 3))
m2_err = cv.glm(Data, m2)
m2_err$delta

m3 = glm(y ~ poly(x, 4))
m3_err = cv.glm(Data, m3)
m3_err$delta

summary(m3)
