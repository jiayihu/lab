import numpy as np
from sklearn.datasets import load_iris
from sklearn.model_selection import train_test_split
from sklearn.naive_bayes import GaussianNB

iris = load_iris()

X_train, X_test, y_train, y_test = train_test_split(iris['data'], iris['target'], test_size = 0.5, random_state = 42)

gnb = GaussianNB().fit(X_train, y_train)
gnb_pred = gnb.predict(X_test)

print("Accuracy Gaussian Naive Bayer: %d points mislabeled" %(y_test != gnb_pred).sum())
