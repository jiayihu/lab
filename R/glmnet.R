library(ISLR)
data("Hitters")
View(Hitters)

# Analisi esplorativa, si vede NA's in Salary
summary(Hitters)
Hitters = na.omit(Hitters)
dim(Hitters)
boxplot(Hitters$Salary)
hist(Hitters$Salary, breaks = 30) # la distribuzione non è simmetrica
hist(log(Hitters$Salary), breaks = 30) # adesso è più simmetrica, anche se non è una normale
Hitters$Salary = log(Hitters$Salary)
y = Hitters$Salary
x = model.matrix(Salary ~ ., data = Hitters)
x = x[, -1]
head(x)
m_ridge = glmnet(x, y, alpha = 0) # alpha = 1 equivale al lasso
plot(m_ridge)
plot(m_ridge, xvar = "lambda")
plot(m_ridge, xvar = "dev")
m_ridge_cv = cv.glmnet(x, y, alpha = 0)
m_ridge_cv$lambda.min # lambda con min MSE
m_ridge_cv$lambda.1se # lambda massimo con min standard deviation
plot(m_ridge_cv) # prima linea verticale corrisponde a lambda.min, il secondo il massimo lambda possibile entro sd minimo
m_ridge_best = glmnet(x, y, alpha = 0, lambda = m_ridge_cv$lambda.min)
m_ridge_best$beta

m_lasso = glmnet(x, y, alpha = 1)
plot(m_lasso, xvar = "lambda")
plot(m_lasso, xvar = "dev")
m_lasso_cv = cv.glmnet(x, y, alpha = 1)
plot(m_lasso_cv)
m_lasso_best = glmnet(x, y, alpha = 1, lambda = m_lasso_cv$lambda.min)
m_lasso_best2 = glmnet(x, y, alpha = 1, lambda = m_lasso_cv$lambda.1se)
m_lasso_best2$beta
