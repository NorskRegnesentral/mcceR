import pandas as pd
from sklearn.datasets import fetch_california_housing, load_iris, load_boston
from sklearn.model_selection import train_test_split


def load_california_housing():
  housing = fetch_california_housing()
  dfx = pd.DataFrame(housing.data, columns=housing.feature_names)
  dfy = pd.DataFrame({'target': housing.target})
  dfx_train, dfx_test, dfy_train, dfy_test = train_test_split(dfx, dfy, test_size=0.99, random_state=42)
  dfx_test, dfy_test = dfx_test[:5], dfy_test[:5] # To reduce computational load
  return dfx_train, dfx_test, dfy_train, dfy_test


def load_binary_iris():
  bcancer = load_iris()
  dfx = pd.DataFrame(bcancer.data, columns=bcancer.feature_names).iloc[bcancer.target<2] # Turning it into a binary classification problem
  dfy = pd.DataFrame({'target': bcancer.target}).iloc[bcancer.target<2] # Turning it into a binary classification problem
  dfx_train, dfx_test, dfy_train, dfy_test = train_test_split(dfx, dfy, test_size=5, random_state=42)
  return dfx_train, dfx_test, dfy_train, dfy_test

def load_boston2():
  boston = load_boston()
  dfx0 = pd.DataFrame(boston.data, columns=boston.feature_names)
  dfx = dfx0.loc[:,["LSTAT","RM","DIS","INDUS"]]
  dfy = pd.DataFrame({'MEDV': boston.target})

  dfx_test = dfx.loc[0:6,]
  dfx_train = dfx.loc[7:507,]

  dfy_test = dfy.loc[0:6,]
  dfy_train = dfy.loc[7:507,]

  return dfx_train, dfx_test, dfy_train, dfy_test

