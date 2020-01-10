from sklearn.neural_network import MLPClassifier
from sklearn.decomposition import PCA
from sklearn.datasets import load_digits
import numpy as np
from sklearn.metrics import mean_squared_error
from sklearn.model_selection import KFold
import matplotlib.pyplot as plt

def classify(X, y):
  k = 5
  kf = KFold(n_splits=k, random_state=42)
  accuracy = np.zeros(k)

  for ifold, indexes in zip(range(k), kf.split(X, y)):
    train_index, test_index = indexes
    X_train, X_test, y_train, y_test = X[train_index], X[test_index], y[train_index], y[test_index]
    clf = MLPClassifier(random_state=42).fit(X_train, y_train)
    pred = clf.predict(X_test)
    accuracy[ifold] = clf.score(X_test, y_test)

  mean_accuracy = np.mean(accuracy)
  return mean_accuracy;

def plot_eigenvalues(eigenvalues):
  plt.plot(np.arange(len(eigenvalues)) + 1, eigenvalues, 'ro-')
  plt.xlabel('Principal component')
  plt.ylabel('Eigenvalue')
  plt.show()

def plot_pc(X_pca):
  plt.scatter(X_pca[:, 0], X_pca[:, 1],
            c=digits['target'], edgecolor='none', alpha=0.5,
            cmap=plt.cm.get_cmap('Paired', 10))
  plt.xlabel('component 1')
  plt.ylabel('component 2')
  plt.colorbar();
  plt.show()

def explore_pca():
  pca = PCA().fit(X)
  eigenvalues = pca.explained_variance_ratio_
  print(eigenvalues)
  # plot_eigenvalues(eigenvalues)
  mean_variance = np.mean(eigenvalues)
  print(mean_variance)
  print(len(eigenvalues[eigenvalues > mean_variance]))

def apply_pca(X):
  pca = PCA(n_components=14, random_state=42).fit(X)
  # print(pca.explained_variance_ratio_)
  X_pca = pca.transform(X)
  return X_pca

digits = load_digits()
X = digits['data']
y = digits['target']
X_pca = apply_pca(X)
accuracy_before = classify(X, y)
accuracy_after = classify(X_pca, y)
print("Accuracy on original X:", accuracy_before)
print("Accuracy on projected X:", accuracy_after)

