from sklearn.neural_network import MLPRegressor
from sklearn.datasets import load_digits
import numpy as np
import matplotlib.pyplot as plt 
from sklearn.metrics import mean_squared_error
from sklearn.model_selection import KFold

Digits = load_digits()
X = Digits.data

def Chart(EDs,Original):
  plt.gray()
  f,axs=plt.subplots(EDs.shape[0],2)  # create all the subplots at the start
  f.set_size_inches(15,20)  
  print(axs.shape)            
  for i in range(axs.shape[0]):
    axs[i][0].matshow(Original[i])
    axs[i][1].matshow(EDs[i])
  plt.show()

num = 5 # number of hidden layer sizes
sizes = np.linspace(4, 64, num).astype(int) # hidden layer sizes
k = 5
alpha = 0
kf = KFold(n_splits=k, random_state=42)

# Each row is the size of the hidden layer and the relative error
errors = np.zeros(shape=(k, num))

for ifold, indexes in zip(range(k), kf.split(X)):
  train_index, test_index = indexes
  X_train, X_test = X[train_index], X[test_index]

  for j in range(num):
    AE = MLPRegressor(hidden_layer_sizes=(sizes[j]), activation='identity', solver='adam')
    AE.fit(X_train, X_train) #Training
    EDs = AE.predict(X_test)
    error = mean_squared_error(X_test, EDs) + alpha * sizes[j]
    errors[ifold][j] = error

average_errors = [(sizes[i], np.mean(errors[:, i])) for i in range(num)]

print("Errors", average_errors)

# Chart(EDs,Digits.images[l:])
