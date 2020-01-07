import numpy as np
from sklearn.datasets import load_iris
from sklearn.model_selection import train_test_split
from sklearn.naive_bayes import GaussianNB
from naive_bayes import NaiveBayes

iris = load_iris()

X_train, X_test, y_train, y_test = train_test_split(iris['data'], iris['target'], test_size = 0.5, random_state = 42)

def accuracy(actual, predicted):
  return (actual != predicted).sum()

nb = NaiveBayes(X_train, y_train)
nb_pred = nb.predict(X_test)
print("Accuracy custom Naive Bayes: %d points mislabeled" %accuracy(y_test, np.array(nb_pred)))

gnb = GaussianNB().fit(X_train, y_train)
gnb_pred = gnb.predict(X_test)
print("Accuracy scikit Naive Bayes: %d points mislabeled" %accuracy(y_test, gnb_pred))
