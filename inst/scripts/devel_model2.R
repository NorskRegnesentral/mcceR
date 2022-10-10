library(data.table)
library(xgboost)

load(file = "/nr/project/stat/Fundingpartner/Martin/mcceR_testData.RData")



devtools::load_all(".")

fit_object <- fit(x_train = head(x_train,5000),
                  pred_train = head(pred_train,5000),
                  fixed_features = fixed_features,
                  c_int = c_int,
                  autoregressive_model = "rpart",
                  seed = 123)



sim_object <- generate(x_explain, fit_object=fit_object, K = 10000, seed=123)

x_sim <- sim_object$simData
pred_sim <- predict_model.ratio(model=model,newdata=x_sim)

cfs <- process(x_sim = x_sim,
               pred_sim = pred_sim,
               x_explain = x_explain,
               fit_object = fit_object,
               processing = c("validation","L0","L1"), # Don't obey this quite yet
               remove_invalid = TRUE,
               return_best_k = 5,
               sort_by_processing_order = TRUE)




cf <- explain_mcce(model = model,
                   x_explain = x_explain,
                   x_train = head(x_train,10^3),
                   predict_model = predict_model.ratio,
                   fixed_features = fixed_features,
                   c_int=c(0,0.1),
                   fit.autoregressive_model="ctree", fit.decision = TRUE, fit.seed = NULL,
                   generate.K = 1000, generate.seed = NULL,
                   process.measures = c("validation","L0","L1"),process.return_best_k = TRUE, process.remove_invalid = TRUE, process.sort_by_measures_order = TRUE)




library(xgboost)
data("Boston", package = "MASS")

x_var <- c("lstat", "rm", "dis", "indus")
y_var <- "medv"

ind_x_test <- 1:6
x_train <- as.matrix(Boston[-ind_x_test, x_var])
y_train <- Boston[-ind_x_test, y_var]
x_test <- as.matrix(Boston[ind_x_test, x_var])

# Fitting a basic xgboost model to the training data
model <- xgboost(
  data = x_train,
  label = y_train,
  nround = 20,
  verbose = FALSE
)

predict(model,x_test)

cf.Boston <- explain_mcce(model = model,
                   x_explain = x_test,
                   x_train = x_train,
                   fixed_features = "lstat",
                   c_int=c(35,1000))




