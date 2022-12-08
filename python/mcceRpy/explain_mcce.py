
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
    fixed_features,
    c_int = np.array([0.5,1]),
    fit_autoregressive_model="ctree", fit_decision = True, fit_seed = None,
    generate_K = 1000, generate_seed = None,
    process_measures = ["validation","L0","L1"], process_return_best_k = 1, 
    process_remove_invalid = True, process_sort_by_measures_order = True):

    pred_train = predict_model(model,x_train)

    feature_names = x_train.columns

    rfit_object = mcceR.fit(
        x_train = py2r(x_train),
        pred_train = py2r(pred_train),
        fixed_features = ro.StrVector(fixed_features),
        c_int = py2r(c_int),
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
        measures = ro.StrVector(process_measures), # Don't obey this quite yet
        remove_invalid = process_remove_invalid,
        return_best_k = process_return_best_k,
        sort_by_measures_order = process_sort_by_measures_order)

    time_vec = {"fit_time" : round(r2py(rfit_object.rx2('time_fit'))[0],4),
                "generate_time" : round(r2py(rsim_object.rx2('time_generate'))[0],4),
                "process_time" : round(r2py(rcfs.rx2('time_process'))[0],4)}


    ret = {"cf": r2py(rcfs.rx2('cf')),
          "fixed_features": r2py(rfit_object.rx2('fixed_features')),
          "time": time_vec} 
          
    return ret
