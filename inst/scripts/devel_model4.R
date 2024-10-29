

devtools::load_all(".")

data(iris)

iris_binary <- as.data.table(iris)
iris_binary[,y:=ifelse(Species=="virginica",0,1)]
iris_binary[,y:=as.factor(y)]
iris_binary[,Sepal.length_cat := as.factor(ifelse(Sepal.Length>5.5,1,0))]

iris_binary[,Species:=NULL]
iris_binary_test <- iris_binary[c(83,53,70,45,44)]
iris_binary_train <- iris_binary[-c(83,53,70,45,44)]


model <- glm(y~.,family = "binomial",data = iris_binary_train)

predict(object = model,newdata=iris_binary_test,type="response")

cf <- explain_mcce(model = model,
                   x_explain = iris_binary_test[,-"y"],
                   x_train = iris_binary_train[,-"y"],
                   process.measures = c("validation","L0","gower"),
                   c_int=c(0,0.5),
                   fit.seed = 123,
                   generate.seed = 123)




#library(xgboost)
data("Boston", package = "MASS")

x_var <- c("lstat", "rm", "dis", "indus")
y_var <- "medv"
xy_var <- c(x_var,y_var)


ind_x_test <- 1:6
xy_train <-Boston[-ind_x_test, xy_var]
x_train <-Boston[-ind_x_test, x_var]
x_test <- Boston[ind_x_test, x_var]


model <- lm(as.formula(paste0(y_var,"~.")),data = xy_train)

predict(model,x_test)

### Manually adjust model parameters to match those in python:

model$coefficients <- c(8.632407012695102,-0.6668123 ,  4.54767491, -0.92003323, -0.24707083)

predict(model,x_test)


cf.Boston <- explain_mcce(model = model,
                          x_explain = x_test,
                          x_train = x_train,
                          fixed_features = "lstat",
                          c_int=c(35,1000),fit.seed = 123,generate.seed = 123)

cf.Boston$


cf.Boston$cf[]


#### Xgbost model on these data as well:
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



