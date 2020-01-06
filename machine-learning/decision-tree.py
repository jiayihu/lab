from sklearn.tree import DecisionTreeClassifier
from sklearn.ensemble import RandomForestClassifier
from sklearn.datasets import load_breast_cancer
from sklearn.model_selection import train_test_split
import matplotlib.pyplot as plt
import numpy as np

cancer = load_breast_cancer()
X_train, X_test, y_train, y_test = train_test_split(cancer.data, cancer.target, stratify = cancer.target, random_state = 42)

tree = DecisionTreeClassifier(max_depth=4, random_state=0)
tree.fit(X_train, y_train)

print("Accuracy on tree training set %.3f" %tree.score(X_train, y_train))
print("Accuracy on tree test set %.3f" %tree.score(X_test, y_test))

# n_features = cancer.data.shape[1]
# plt.barh(range(n_features), tree.feature_importances_, align="center")
# plt.yticks(np.arange(n_features), cancer.feature_names)
# plt.xlabel("Feature importance")
# plt.ylabel("Feature")
# plt.show()

forest = RandomForestClassifier(n_estimators=1 00, random_state=0)
forest.fit(X_train, y_train)

print("Accuracy on forest training set %f" %forest.score(X_train, y_train))
print("Accuracy on forest test set %f" %forest.score(X_test, y_test))
