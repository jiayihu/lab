from sklearn.datasets import load_iris
from sklearn.model_selection import train_test_split
import pandas as pd
import matplotlib.pyplot as plt
import numpy as np


iris = load_iris()
X_train, X_test, y_train, y_test = train_test_split(iris['data'], iris['target'], random_state = 0)

# %matplotlib inline

iris_df = pd.DataFrame(X_train, columns = iris['feature_names'])
grr = pd.plotting.scatter_matrix(iris_df, c = y_train, figsize = (15, 15), marker = 'o', hist_kwds = {'bins': 20}, s = 60, alpha = 0.8)
# plt.show()

from sklearn.neighbors import KNeighborsClassifier

knn = KNeighborsClassifier(n_neighbors = 1)
knn.fit(X_train, y_train)

X_new = np.array([[5, 2.9, 1, .2]])
prediction = knn.predict(X_new)
print(iris['target_names'][prediction])

y_pred = knn.predict(X_test)
print(np.mean(y_pred == y_test))
print(knn.score(X_test, y_test))
