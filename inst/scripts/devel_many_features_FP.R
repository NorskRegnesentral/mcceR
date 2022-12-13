## Read data received from Fundingpartner January 2022
##

library(data.table)
library(xgboost)
library(pROC)


path <- "/disk/home/jullum/nr/project_stat/Fundingpartner/Martin"

data <- fread(file.path(path,"modelling_data.csv"))
folds <- unlist(fread(file.path(path,"fold_split.csv")),use.names = F)

data[,V1:=NULL]

these_infinite <- which(unlist(data[,lapply(.SD,function(x){any(is.infinite(x))})]))
which(unlist(data[,lapply(.SD,function(x){any(is.nan(x))})]))
which(unlist(data[,lapply(.SD,function(x){any(is.na(x))})]))

max_vals <- unlist(data[,lapply(.SD,function(x)max(x[!is.infinite(x)])),.SDcols=these_infinite])


for(i in seq_along(these_infinite)){
  col_i <- names(these_infinite)[i]
  max_val_i <- max_vals[i]
  data[is.infinite(get(col_i)),(col_i) :=max_val_i+1]
}


Xmodel.matrix.all <- as.matrix(data[,-"bankrupt"])#model.matrix(~.,dt.xgb[,-"y"])
y.all <- unlist(data[,"bankrupt"],use.names = F)
xgbMatrix.all <- xgb.DMatrix(data = Xmodel.matrix.all,label = y.all)

unique_folds <- sort(unique(folds))
test_chunks <- list()
for (i in seq_along(unique_folds)){

  test_chunks[[i]] <-  which(folds==unique_folds[i])

}

names(test_chunks) <- unique_folds



source(file.path(path,"xgboost_runner.R"))


all_params_list <- list(params = list(eta = 0.3,#0.15,
                                         max_depth = 7,
                                         min_child_weight=4,
                                         gamma=0.15,
                                         subsample=0.8,
                                         colsample_bytree=0.98,#var 0.8
                                         colsample_bylevel=1,#hadde ikke med denne
                                         lambda = 100,
                                         alpha = 1,
                                         scale_pos_weight = 1,
                                         max_delta_step = 0,
                                         objective= 'binary:logistic',
                                         eval_metric = c("auc"),
                                         tree_method = "gpu_hist"),
                           early_stopping_rounds = 30,
                           print_every_n = 30,
                           nrounds = 10,
                           nthread = 12,
                           verbose=TRUE)

set.seed(111)
vector <- seq_len(nrow(xgbMatrix.all))
no_chunks <- 2
test_chunks <- split(sample(vector,size = length(vector),replace = F),  cut(seq_along(vector), no_chunks, labels = FALSE))

test.set <- test_chunks[[1]]
train.set <- unlist(test_chunks[-1])

xgbMatrix.train <- slice(xgbMatrix.all,idxset = train.set)
xgbMatrix.test <- slice(xgbMatrix.all,idxset = test.set)

y.test <- xgboost::getinfo(xgbMatrix.test,name="label")


xgbFit.plain <- xgb.train(data=xgbMatrix.train,
                          params = all_params_list$params,
                          nrounds = 100,  #Using the best iteration from the cv-fit
                          watchlist = list(train = xgbMatrix.train, # train
                                           test = xgbMatrix.test), # Just for fun
                          print_every_n = all_params_list$print_every_n,
                          nthread = all_params_list$nthread,
                          verbose = all_params_list$verbose)

x_train <- Xmodel.matrix.all[train.set,]
x_explain <- Xmodel.matrix.all[test.set,]

all.equal(predict(xgbFit.plain,xgbMatrix.test),
          predict(xgbFit.plain,x_explain))

summary(predict(xgbFit.plain,x_explain))

x_explain <- head(x_explain,20)
x_train <- head(x_train,1000)

devtools::load_all(".")
explained <- explain_mcce(model = xgbFit.plain,
                          x_explain = x_explain,
                          x_train = x_train,
                          c_int = c(0.2,1),
                          predict_model=NULL,
                          fixed_features = NULL,fit.decision = FALSE)

predict(xgbFit.plain,as.matrix(explained$cf[,-c(1,2)]))

# OK, this works.


