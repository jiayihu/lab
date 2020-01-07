import numpy as np
from scipy import stats

def get_instances_by_class(X, y):
  classes = dict()

  for i in range(len(y)):
    instance = X[i]
    class_value = y[i]

    if (class_value not in classes):
      classes[class_value] = list()
    
    classes[class_value].append(instance)
    
  return classes

def get_statistics(X):
  statistics = [(np.mean(column), np.std(column), len(column)) for column in zip(*X)]
  return statistics

def get_class_statistics(X, y):
  instances_by_class = get_instances_by_class(X, y)
  class_statistics = dict()
  for class_value, instances in instances_by_class.items():
    class_statistics[class_value] = get_statistics(instances)

  return class_statistics

def get_probabilities(statistics, input):
  N = sum([statistics[class_value][0][2] for class_value in statistics])
  probabilities = dict()

  for class_value, class_stats in statistics.items():
    probabilities[class_value] = class_stats[0][2] / N # likelihood

    for i in range(len(class_stats)):
      mean, stdev, count = class_stats[i]
      # Assuming Gaussian distribution for inputs
      probabilities[class_value] *= stats.norm(mean, stdev).pdf(input[i])

  return probabilities

class NaiveBayes:
  def __init__(self, X, y):
    self.statistics = get_class_statistics(X, y)
  
  def predict_single(self, input):
    probabilities = get_probabilities(self.statistics, input)
    best_class, max_prob = None, -1

    for class_value, prob in probabilities.items():
      if (prob > max_prob):
        best_class = class_value
        max_prob = prob

    return best_class

  def predict(self, test):
    predictions = list()
    for row in test:
      predictions.append(self.predict_single(row))
    
    return predictions
