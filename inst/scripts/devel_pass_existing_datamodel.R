data("airquality")
airquality <- airquality[complete.cases(airquality), ]

x_var <- c("Solar.R", "Wind", "Temp", "Month")
y_var <- "Ozone"

ind_x_explain <- 1:6
x_train <- as.matrix(airquality[-ind_x_explain, x_var])
y_train <- airquality[-ind_x_explain, y_var]
x_explain <- as.matrix(airquality[ind_x_explain, x_var])

# Fitting a basic xgboost model to the training data
model <- xgboost::xgboost(
  data = x_train,
  label = y_train,
  nround = 20,
  verbose = FALSE
)

#predict(model,x_train)

explained <- explain_mcce(model = model,
                          x_explain = x_explain,
                          x_train = x_train,
                          c_int = c(-Inf,15),
                          predict_model=NULL,
                          fixed_features = "Wind",
                          fit.seed = 123,
                          generate.seed = 123,
                          return_featuremodel_object  = TRUE)


explained2 <- explain_mcce(model = model,
                          x_explain = x_explain,
                          x_train = "blabla",
                          c_int = c(-Inf,15),
                          featuremodel_object = explained$featuremodel_object,
                          predict_model=NULL,
                          fixed_features = "Wind",
                          fit.seed = 123,
                          generate.seed = 123,
                          return_featuremodel_object  = TRUE)
