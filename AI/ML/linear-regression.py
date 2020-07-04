from sklearn.linear_model import LinearRegression, Ridge, Lasso
from sklearn.model_selection import train_test_split
from sklearn.datasets import load_boston
from sklearn.preprocessing import MinMaxScaler, PolynomialFeatures
import numpy as np
import matplotlib.pyplot as plt

boston = load_boston()
X = boston.data
X = MinMaxScaler().fit_transform(boston.data)
X = PolynomialFeatures(degree=2, include_bias=False).fit_transform(X)
y = boston.target

X_train, X_test, y_train, y_test = train_test_split(X, y, random_state = 42)

lr = LinearRegression().fit(X_train, y_train)

# print(lr.coef_)
# print(lr.intercept_)
print("Linear regression Training set score: %.3f" %lr.score(X_train, y_train))
print("Linear regression Test set score %.3f" %lr.score(X_test, y_test))

ridge = Ridge().fit(X_train, y_train)
print("Ridge regression Training set score: %.3f" %ridge.score(X_train, y_train))
print("Ridge regression Test set score %.3f" %ridge.score(X_test, y_test))

ridge10 = Ridge(alpha=10).fit(X_train, y_train)
ridge01 = Ridge(alpha=.1).fit(X_train, y_train)

plt.plot(ridge.coef_, 's', label="Ridge alpha=1")
plt.plot(ridge10.coef_, '^', label="Ridge alpha=10")
plt.plot(ridge01.coef_, 'v', label="Ridge alpha=0.1")

# plt.plot(lr.coef_, 'o', label="LinearRegression")
# plt.xlabel("Coefficient index")
# plt.ylabel("Coefficient magnitude")
# plt.hlines(0, 0, len(lr.coef_))
# plt.ylim(-25, 25)
# plt.legend()
# plt.show()

lasso = Lasso(alpha=0.01, max_iter=100000).fit(X_train, y_train)
print("Lasso regression Training set score: %.3f" %lasso.score(X_train, y_train))
print("Lasso regression Test set score %.3f" %lasso.score(X_test, y_test))
print("Number of features used: %d" %np.sum(lasso.coef_ != 0))
