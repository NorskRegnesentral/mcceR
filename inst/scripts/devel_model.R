library(data.table)
library(xgboost)

#Laster inn funksjonene computeRatios,find_indices og distFuncNy
source("/nr/project/stat/Fundingpartner/Counterfactuals/MCCE/utility.R")

#Laster inn data og XGBoost modell
load("/nr/project/stat/Fundingpartner/Source/v4DataJan22/2022-06-01objekterTilKjerstiutenbransje.RData")
load("/nr/project/stat/Fundingpartner/Source/v4DataJan22/2022-05-30komplettData2aarTilKjersti.RData")

model <- xgb.ratio$fit

testData  <- df.xgb[test.set,]
xgb.test  <- xgb.DMatrix(data = as.matrix(sapply(testData[,-c(1,38)], as.numeric)))
results0  <- predict(xgb.ratio$fit,xgb.test)


indAll <- find_indices(df.komplett)
x_train <- as.data.table(df.komplett[,indAll])


pantVariables     <- c("Pant.Inventory","Pant.Operating.assets.without.intangible","Pant.Operating.assets.with.intangible","Pant.Factoring","Pant.total")
boardVariables    <- c("ceo_age","bc_age","bc_n_bc","bc_n_bm")
employVariables   <- c("n_emp","per_change_3m","per_change_6m","per_change_12m")
bankruptVariables <- c("negative_total_equity","bankruptcy_defendant")
nn_underlying     <- colnames(x_train)[17:36]
accountVariables  <- nn_underlying[c(12,20,6,7,2,5,8,19)]
balanceVariables  <- nn_underlying[c(10,1,15,3)]


fixed_features <- c("Age", pantVariables, boardVariables, employVariables,accountVariables,balanceVariables, bankruptVariables)

ind0 <- rev(order(results0))[1:20]

x_explain <- setDT(x_train[ind0])


predict_model.ratio <- function(model,newdata){
  newdata_pred <- computeRatios(newdata,df.komplett)[,model$feature_names]
  predict(model,as.matrix(newdata_pred))
}

pred_train <- predict_model.ratio(model=model,newdata=x_train)



pred_explain <- predict_model.ratio(model=model,newdata=x_explain)

c_int = c(0.1,1)


save(list = c("model","pred_train","x_train","fixed_features","c_int","x_explain","pred_explain","predict_model.ratio","computeRatios","df.komplett"),
     file = "/nr/project/stat/Fundingpartner/Martin/mcceR_testData.RData")

