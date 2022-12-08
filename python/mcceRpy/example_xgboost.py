from mcceRpy import explain_mcce 
from mcceRpy import datasets

#### Run python code ####

import xgboost as xgb
import numpy as np

dfx_train, dfx_test, dfy_train, dfy_test = datasets.load_california_housing()

## Fit model
dtrain = xgb.DMatrix(data=dfx_train, label=dfy_train)
model = xgb.train(params={}, num_boost_round=20, dtrain=dtrain)

def xgb_predict_model(model,newdata):
  return model.predict(xgb.DMatrix(newdata))

a=xgb_predict_model(model,dfx_test)

cf_test = explain_mcce.explain_mcce(
  model = model,
  x_explain = dfx_test,
  x_train = dfx_train,
  predict_model = xgb_predict_model,
  fixed_features = ["MedInc"],
  c_int = np.array([2,100]),
  fit_autoregressive_model="ctree", fit_decision = True, fit_seed = None,
  generate_K = 1000, generate_seed = None,
  process_measures = ["validation","L0","L1"], process_return_best_k = 1,
  process_remove_invalid = True, process_sort_by_measures_order = True)


cf_test
