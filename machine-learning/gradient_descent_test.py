import pandas as pd
from sklearn.linear_model import LinearRegression
from sklearn.model_selection import train_test_split
from sklearn.preprocessing import normalize
from sklearn.metrics import r2_score
from sklearn.datasets import make_regression
from gradient_descent import gradient_descent, predict

X, y = make_regression(1000, 1, random_state=42)

X_train, X_test, y_train, y_test = train_test_split(X, y, random_state=42)

weights = gradient_descent(X, y)
print(weights)
print("Gradient descent Training set score: %.3f" %r2_score(y_train, predict(weights, X_train)))
print("Gradient descent Test set score %.3f" %r2_score(y_test, predict(weights, X_test)))
print("Intercept {}, coefs {},".format(weights[0], weights[1:]))

lr = LinearRegression().fit(X_train, y_train)

print("Linear regression Training set score: %.3f" %r2_score(y_train, lr.predict(X_train)))
print("Linear regression Test set score %.3f" %r2_score(y_test, lr.predict(X_test)))
print("Intercept {}, coefs {},".format(lr.intercept_, lr.coef_))
