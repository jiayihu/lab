import numpy as np

def predict(weights, X):
  predictions = [0 for x in range(len(X))]

  for i in range(len(X)):
    predictions[i] = weights[0] + np.dot(weights[1:], X[i])

  return predictions

def get_error(weights, X, y):
  predictions = predict(weights, X);
  error = (1 / (2 * len(X))) * np.sum(np.square(y - predictions))

  return error

def get_derivative(i, X, y, predictions):
  result = 0

  for d in range(len(X)):
    if (i == 0): # Intercept
      result += (y[d] - predictions[d])
    else:
      result += (y[d] - predictions[d]) * -(X[d][i - 1])

  return result


def gradient_step(weights, X, y, eta):
  predictions = predict(weights, X)
  new_weights = [x for x in range(len(weights))]

  for weight, i in zip(weights, range(len(weights))):
    derivative = get_derivative(i, X, y, predictions)
    new_weights[i] = weights[i] - (1 / len(X)) * eta * derivative

  return new_weights


def gradient_descent(X, y):
  weights = [0] + [0 for x in range(len(X[0]))] # Intercept
  iterations = 1000
  eta = 0.01
  error = get_error(weights, X, y)

  for i in range(iterations):
    new_weights = gradient_step(weights, X, y, eta)

    while (get_error(new_weights, X, y) > error):
      eta = eta / 2
      new_weights = gradient_step(weights, X, y, eta)
    
    error = get_error(new_weights, X, y)
    weights = new_weights

    print("Error", error)

  return weights
