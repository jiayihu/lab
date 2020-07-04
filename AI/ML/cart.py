import math
from sklearn.datasets import load_iris
from sklearn.tree import DecisionTreeClassifier
from sklearn.model_selection import train_test_split
from sklearn import tree
import numpy as np
from sklearn.model_selection import KFold
import matplotlib.pyplot as plt

def gini_index(groups, classes):
  Nm = sum([len(group) for group in groups])
  gini = 0

  for group in groups:
    Nmj = len(group)

    if Nmj == 0:
      continue

    score = 0
    for class_value in classes:
      group_classes = [row[-1] for row in group]
      p = group_classes.count(class_value) / Nmj
      score += p*p
    
    gini += (1 - score) * (Nmj / Nm)

  return gini

def split_by_attribute_value(index, value, dataset):
  left, right = list(), list()
  
  for row in dataset:
    if row[index] < value:
      left.append(row)
    else:
      right.append(row)

  return left, right

def get_best_split(dataset):
  classes = list(set(row[-1] for row in dataset))
  b_index, b_value, b_score, b_groups = 999, 999, 999, None

  for index in range(len(dataset[0]) - 1):
    for row in dataset:
      groups = split_by_attribute_value(index, row[index], dataset)
      gini = gini_index(groups, classes)
      if gini < b_score:
        b_index = index
        b_value = row[index]
        b_score = gini
        b_groups = groups

  return {'index': b_index, 'value': b_value, 'groups': b_groups}

def get_leaf(group):
  classes = [row[-1] for row in group]
  return max(set(classes), key = classes.count)

def split_node(node, max_depth, min_size, depth):
  left, right = node['groups']
  del(node['groups'])

  if not left or not right:
    node['left'] = node['right'] = get_leaf(left + right)
    return

  if depth >= max_depth:
    node['left'], node['right'] = get_leaf(left), get_leaf(right)
    return

  if len(left) <= min_size:
    node['left'] = get_leaf(left)
  else:
    node['left'] = get_best_split(left)
    split_node(node['left'], max_depth, min_size, depth + 1)
  
  if len(right) <= min_size:
    node['right'] = get_leaf(right)
  else:
    node['right'] = get_best_split(right)
    split_node(node['right'], max_depth, min_size, depth + 1)

class CART:
  def __init__(self, dataset, max_depth = math.inf, min_size = 1):
    self.root = get_best_split(dataset)
    split_node(self.root, max_depth, min_size, 1)

  def print(self):
    self.print_node(self.root, 0)

  def print_node(self, node, depth = 0):
    if isinstance(node, dict):
      print('%s[X%d < %.3f]' % (depth * ' ', node['index'] + 1, node['value']))
      print_node(node['left'], depth + 1)
      print_node(node['right'], depth + 1)
    else:
      print('%s[%s]' % (depth * ' ', node))

  def predict(self, test):
    predictions = list()
    for row in test:
      predictions.append(self.predict_node(self.root, row))
    
    return predictions

  def predict_node(self, node, row):
    node = node['left'] if row[node['index']] < node['value'] else node['right']
    return self.predict_node(node, row) if isinstance(node, dict) else node


## Testing the classifier

def accuracy(actual, predicted):
  return (actual != predicted).sum()
  
X, y = load_iris(return_X_y=True)

k = 5
kf = KFold(n_splits=k, random_state=42)
errors = {'custom': np.zeros(k), 'scikit': np.zeros(k)}

for ifold, indexes in zip(range(k), kf.split(X, y)):
  train_index, test_index = indexes
  X_train, X_test, y_train, y_test = X[train_index], X[test_index], y[train_index], y[test_index]

  # Add output as last column to input
  dataset = np.hstack((X_train, np.reshape(y_train, (-1, 1))))
  cart = CART(dataset)
  test = np.hstack((X_test, np.reshape(y_test, (-1, 1))))
  cart_pred = cart.predict(test)
  errors['custom'][ifold] = accuracy(y_test, cart_pred)

  clf = DecisionTreeClassifier(criterion="gini", random_state=42).fit(X_train, y_train)
  clf_pred = clf.predict(X_test)
  errors['scikit'][ifold] = accuracy(y_test, clf_pred)

print("Custom CART average misclassifications:", np.mean(errors['custom']))
print("Scikit CART average misclassifications:", np.mean(errors['scikit']))


## Plot
X = X[:, 2:4]
n_classes = 3
plot_colors = "brk"
plot_step = 0.02
x_min, x_max = X[:, 0].min() - 1, X[:, 0].max() + 1
y_min, y_max = X[:, 1].min() - 1, X[:, 1].max() + 1
xx, yy = np.meshgrid(np.arange(x_min, x_max, plot_step),
                      np.arange(y_min, y_max, plot_step))

dataset = np.hstack((X, np.reshape(y, (-1, 1))))
cart = CART(dataset, min_size=0.05 * len(dataset))
clf = DecisionTreeClassifier(criterion="gini", random_state=42).fit(X, y)
Z = cart.predict(np.c_[xx.ravel(), yy.ravel()])
Z = np.array(Z).reshape(xx.shape)
cs = plt.contourf(xx, yy, Z, cmap=plt.cm.Paired, alpha = 0.4)

plt.xlabel('Petal length')
plt.ylabel('Petal width')
plt.axis("tight")

# Plot the training points
for i, color in zip(range(n_classes), plot_colors):
  idx = np.where(y == i)
  plt.scatter(X[idx, 0], X[idx, 1], c=color)

plt.show()
