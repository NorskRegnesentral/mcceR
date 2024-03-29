### Testing on boston dataset to try to get same results in both R and python
from mcceRpy import explain_mcce 
from mcceRpy import datasets

dfx_train, dfx_test, dfy_train, dfy_test = dfx_train, dfx_test, dfy_train, dfy_test = datasets.load_california_housing()


from sklearn import linear_model

model = linear_model.LinearRegression()
model.fit(dfx_train,dfy_train.iloc[:,0])

model.intercept_
model.coef_

def lm_predict_model(model,newdata):
    return model.predict(newdata)

lm_predict_model(model,dfx_test)

cf_Boston = explain_mcce.explain_mcce(
    model = model,
    x_explain = dfx_test,
    x_train = dfx_train,
    predict_model = lm_predict_model, 
    fixed_features = ["LSTAT"],
    c_int=np.array([35,1000]),
    fit_seed=123,generate_seed=123)

cf_Boston
