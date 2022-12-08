# mcceRpy

<!-- badges: start -->
<!-- badges: end -->

mcceRpy is a Python wrapper package for the R-package mcceR, built using the rpy2 python library (https://github.com/rpy2/rpy2)

## Installation

Being a python wrapper for an R-package, this package requires installation of both R and quite a few R-packages to function.

There are several ways of installing R, depending on your system. 
Official instructions can be found her (https://cran.r-project.org/)
R can also be installed with pip as follows:
```
pip install rbase
```
and conda:
```
conda install -c r r
```

Once R is installed, you need to install the ´mcceR´ R-package, in addition to quite a few more common R-packages. From the folder of this file, these can be installed as

```
Rscript install_r_packages.R
```

On the Python side you need *rpy2* library, installed with e.g.
```
pip install rpy2
```
and a few other basic packages like *numpy* and *pandas*.

Finally, install the *mcceRpy* library from the present folder:

```
pip install -e .
```

## Example

Here is a basic example of using the *mcceRpy* library

```
from mcceRpy import explain_mcce 
from mcceRpy import datasets

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
```



