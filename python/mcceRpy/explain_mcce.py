
#### Initials start ####
import rpy2.robjects as ro
from rpy2.robjects import numpy2ri
from rpy2.robjects import pandas2ri
from rpy2.robjects.conversion import localconverter

def py2r(obj):
  with localconverter(ro.default_converter + ro.numpy2ri.converter + ro.pandas2ri.converter):
    robj = ro.conversion.py2rpy(obj)
  return robj


def r2py(robj):
  with localconverter(ro.default_converter + ro.numpy2ri.converter + ro.pandas2ri.converter):
      obj = ro.conversion.rpy2py(robj)
  return obj

def maybe_null(val):
    return val if val is not None else NULL


#### Initials ends ####

import pandas as pd
import numpy as np
from rpy2.rinterface import NULL, NA

# R-packages
from rpy2.robjects.packages import importr
data_table = importr('data.table')
utils = importr('utils')
#xgboost = importr('xgboost')
mcceR = importr('mcceR')
base = importr('base')


def explain_mcce(
    model, 
    x_explain, 
    x_train, 
    predict_model, 
    featuremodel_Robject = None, 
    fixed_features = None,
    c_int = np.array([0.5,1]),
    fit_autoregressive_model="ctree", fit_decision = True, fit_seed = None,
    generate_K = 1000, generate_seed = None,
    process_measures = ["validation","L0","L1"],
    process_return_best_k = 1, 
    process_remove_invalid = True,
    process_sort_by_measures_order = True,
    return_featuremodel_Robject = False):
      
    """Explain predictions with Monte Carlo Counterfactual Explanations (MCCE) 

    Parameters
    ----------
    model : 
        The model whose predictions we want to explain.
    x_explain : pandas DataFrame.
        Contains the data that we want to generate counterfactuals for.
    x_train : pandas DataFrame.
        Contains the data used to train the autoregressive feature dependence model.
    predict_model : Function.
        The function must have two arguments, `model` and `newdata` which specify, respectively, the model
        and a data.frame/data.table to compute predictions for. The function must give the prediction as a numeric vector.
    featuremodel_Robject : R-object.
        R object being returned by `explain_mcce()` when `return_featuremodel_Robject` is `True`, and contains the 
        models used to fit the feature distribution.
        Once passed, these models are used to resemble the data distribution 
        (and `x_train`, `fixed_features`, `c_int`, `fit_autoregressive_model`, `fit_decision` and `fit_seed` is ignored).
        `None` (default) means new feature models are fitted based on `x_train`.
    fixed_features : Character vector.
        Names of features to fix in counterfactual generation. Set to None in order to not fix any features
    c_int : Numeric vector (length 2).
        Contains the data used to train the autoregressive feature dependence model.
    fit.autoregressive_model : Character.
        Specifies the name of the autoregressive model used to fit the data.
    fit.decision : Logical.
        Whether to include the decision threshold as a binary features to improve the efficiency of MCCE.
    fit.seed : Positive integer.
        Specifies the seed used when fitting the autoregressive feature dependence model.
        If `NULL` the seed will be inherited from the calling environment.
    generate.K : Numeric.
        Number of potential counterfactuals to generate per row in x_explain.
    generate.seed : Positive integer.
        Specifies the seed used when generating the potential counterfactuals.
        If `NULL` the seed will be inherited from the calling environment.
    process.measures : Character vector.
        Indicates the measures (in the given order) that are applied to the simulate data.
    process.return_best_k : Integer.
        How many counterfactuals should be generated per prediction to explain (at max).
    process.remove_invalid : Logical.
        Indicates whether invalid counterfactuals should be removed from the returned counterfactual list.
    process.sort_by_measures_order : Logical.
        Indicates whether the counterfactuals should be sorted.
    return_featuremodel_Robject : Logical.
        Indicates whether the list of models used to fit the feature distribution should be stored and returned to the user.

    Returns
    -------
    cf
        pandas DataFrame with the generated counterfactual explanations
    time
        timing of the three different parts of the algorithm
    fixed_features
        character vector with the fixed features
    """
      
      

    pred_train = predict_model(model,x_train)

    feature_names = x_train.columns

    rfit_object = mcceR.fit(
        x_train = py2r(x_train),
        pred_train = py2r(pred_train),
        fixed_features = ro.StrVector(maybe_null(fixed_features)),
        c_int = py2r(c_int),
        featuremodel_object = maybe_null(featuremodel_Robject),
        autoregressive_model  = fit_autoregressive_model,
        decision = fit_decision,
        seed = maybe_null(fit_seed))
        
    rsim_object = mcceR.generate(
        x_explain = py2r(x_explain),
        fit_object=rfit_object,
        K = generate_K,
        seed=maybe_null(generate_seed))

    x_sim = r2py(rsim_object.rx2('simData'))

    pred_sim = predict_model(model,x_sim.loc[:,feature_names])  

    rcfs = mcceR.process(
        x_sim = py2r(x_sim),
        pred_sim = py2r(pred_sim),
        x_explain = py2r(x_explain),
        fit_object = rfit_object,
        measures = ro.StrVector(process_measures), 
        remove_invalid = process_remove_invalid,
        return_best_k = process_return_best_k,
        sort_by_measures_order = process_sort_by_measures_order)

    time_vec = {"fit_time" : round(r2py(rfit_object.rx2('time_fit'))[0],4),
                "generate_time" : round(r2py(rsim_object.rx2('time_generate'))[0],4),
                "process_time" : round(r2py(rcfs.rx2('time_process'))[0],4)}


    ret = {"cf": r2py(rcfs.rx2('cf')),
          "fixed_features": r2py(rfit_object.rx2('fixed_features')),
          "time": time_vec} 

    if return_featuremodel_Robject is True:
       ret["featuremodel_Robject"] = rfit_object
          
    return ret
