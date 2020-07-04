from sklearn.datasets import load_digits
from sklearn.model_selection import train_test_split
from sklearn.cluster import KMeans
import matplotlib.pyplot as plt
import numpy as np
from math import ceil, sqrt

def assign_labels_to_centroids(y_clusters, centroids):
  labelled_centroids = list()
  for i in range(len(centroids)):
    counts = np.bincount(y_clusters[i])
    label = np.argmax(counts)
    labelled_centroids.append((centroids[i], label))

  labelled_centroids.sort(key=lambda x:x[1])
  return labelled_centroids

def plot_cluster_labels(labelled_centroids):
  fig = plt.figure(1, figsize=(10,10))
  plt.gray()
  size = ceil(sqrt(len(labelled_centroids)))
  for k in range(len(labelled_centroids)):
    (digit, label) = labelled_centroids[k]

    ax = fig.add_subplot(size, size, k + 1)
    ax.imshow(digit.reshape(8, 8))
    ax.axis('off')
    ax.set_title("Estimated label: " + str(label))

  plt.show()

def accuracy(actual, predicted):
  return (actual != predicted).sum() / len(actual)

def cross_validate():
  X, y = load_digits(return_X_y=True)
  X_train, X_test, y_train, y_test = train_test_split(X, y, random_state = 42)

  errors = [0 for x in range(10, 25)]
  for k in range(10, 25):
    kmeans = KMeans(n_clusters=k, random_state=42).fit(X_train)
    centroids = kmeans.cluster_centers_;
    y_clusters = np.array([y_train[kmeans.labels_ == i] for i in range(k)])
    labelled_centroids = assign_labels_to_centroids(y_clusters, centroids)
    pred = [labelled_centroids[cluster][1] for cluster in kmeans.predict(X_test)]
    errors[k-10] = accuracy(y_test, pred)

    print(errors)
    plt.plot(range(10, 25), errors)
    plt.xlabel('k')
    plt.ylabel('Error rate')
    plt.show()

def test_k(k):
  X, y = load_digits(return_X_y=True)
  X_train, X_test, y_train, y_test = train_test_split(X, y, random_state = 42)
  kmeans = KMeans(n_clusters=k, random_state=42).fit(X_train)
  centroids = kmeans.cluster_centers_;
  y_clusters = np.array([y_train[kmeans.labels_ == i] for i in range(k)])
  labelled_centroids = assign_labels_to_centroids(y_clusters, centroids)
  plot_cluster_labels(labelled_centroids)

test_k(21)
