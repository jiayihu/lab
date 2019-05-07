set.seed(1)
x = rnorm(100)
y = 2 * x + rnorm(100)
m = lm(y~x+0)
summary(m)

summary(lm(x~y+0))
