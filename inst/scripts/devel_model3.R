

devtools::load_all(".")

data(iris)

iris_binary <- as.data.table(iris)
iris_binary[,y:=ifelse(Species=="virginica",0,1)]
iris_binary[,y:=as.factor(y)]
iris_binary[,Species:=NULL]
iris_binary_test <- iris_binary[c(83,53,70,45,44)]
iris_binary_train <- iris_binary[-c(83,53,70,45,44)]


model <- glm(y~.,family = "binomial",data = iris_binary_train)

model





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


cf.Boston <- explain_mcce(model = model,
                          x_explain = x_test,
                          x_train = x_train,
                          fixed_features = "lstat",
                          c_int=c(35,1000),fit.seed = 123,generate.seed = 123)


cf.Boston$cf[]

