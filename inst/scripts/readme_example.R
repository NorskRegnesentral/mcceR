#library(mcceR)
library(xgboost)
## basic example code

data("airquality")
airquality <- airquality[complete.cases(airquality), ]

x_var <- c("Solar.R", "Wind", "Temp", "Month")
y_var <- "Ozone"

ind_x_explain <- 1:6
x_train <- as.matrix(airquality[-ind_x_explain, x_var])
y_train <- airquality[-ind_x_explain, y_var]
x_explain <- as.matrix(airquality[ind_x_explain, x_var])

# Fitting a basic xgboost model to the training data
model <- xgboost(
  data = x_train,
  label = y_train,
  nround = 20,
  verbose = FALSE
)

#predict(model,x_train)

set.seed(123)
explained <- explain_mcce(model = model,
                          x_explain = x_explain,
                          x_train = x_train,
                          c_int = c(-Inf,15),
                          predict_model=NULL,
                          fixed_features = "Wind",
                          return_featuremodel_object = T,
                          return_sim_data = T)


explained$cf



### Controlling the fit procedure and saving the model and simulated data
set.seed(123)
explained <- explain_mcce(model = model,
                          x_explain = x_explain,
                          x_train = x_train,
                          c_int = c(-Inf,15),
                          predict_model=NULL,
                          fixed_features = "Wind",
                          return_featuremodel_object = T,
                          return_sim_data = T,
                          controls = party::ctree_control(stump = TRUE)) # fit trees with only 1 split for testing purpose

explained$model_list
explained$sim_data

