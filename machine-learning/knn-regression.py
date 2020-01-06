import matplotlib.pyplot as plt
import numpy as np
from sklearn.neighbors import KNeighborsRegressor
from sklearn.model_selection import train_test_split

def make_wave(n_samples=100):
  rnd = np.random.RandomState(42)
  x = rnd.uniform(-3, 3, size=n_samples)
  y_no_noise = (np.sin(4 * x) + x)
  y = (y_no_noise + rnd.normal(size=len(x))) / 2
  return x.reshape(-1, 1), y

X, y = make_wave(n_samples=40)
X_train, X_test, y_train, y_test = train_test_split(X, y, random_state = 0)

fix, axes = plt.subplots(1, 3, figsize=(15, 4))

line = np.linspace(-3, 3, 1000).reshape(-1, 1)

for n_neighbors, ax in zip([1, 3, 9], axes):
  reg = KNeighborsRegressor(n_neighbors=n_neighbors)
  reg.fit(X_train, y_train)
  ax.plot(line, reg.predict(line))
  ax.plot(X_train, y_train, '^')
  ax.plot(X_test, y_test, 'v')

  ax.set_title( "{} neighbor(s)\n train score: {:.2f} test score: {:.2f}".format( n_neighbors, reg.score(X_train, y_train), reg.score(X_test, y_test)))
  ax.set_xlabel("Feature")
  ax.set_ylabel("Target")

axes[0].legend(["Model predictions", "Training data/target", "Test data/target"], loc="best")
plt.show()
