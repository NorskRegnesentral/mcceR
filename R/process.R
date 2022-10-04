#' Generate potential counterfactuals from fitted autoregressive model
#'
#' @param x_sim data.table.
#' Contains the generated potential  counterfactuals, including an id-column (id_explain).
#' The output from generate()$x_sim
#'
#' @param pred_sim Numeric vector.
#' The prediction score for the data in x_sim.
#'
#' @param fit_object List.
#' The output from calling the fit() function.
#'
#' @param measures Character vector.
#' Indicates the measures (in the given order) that are applied to the simulate data.
#'
#' @param return_best_k Integer.
#' How many counterfactuals should be generated per prediction to explain (at max).
#'
#' @param remove_invalid Logical.
#' Indicates whether invalid counterfactuals should be removed from the returned counterfactual list.
#'
#' @param sort_by_measures_order Logical.
#' Indicates whether the counterfactuals should be sorted.
#'
#' @inheritParams explain_mcce
#'
#' @return List.
#' Procesed counterfactuals, and timing
#'
#' @export
#'
process = function(x_sim,
                   pred_sim,
                   x_explain,
                   fit_object,
                   measures = c("validation","L0","L1"), # Don't obey this quite yet
                   remove_invalid = TRUE,
                   return_best_k = 1,
                   sort_by_measures_order = TRUE){ # Don't obey this quite yet

  mutable_features <- fit_object$mutable_features
  fixed_features <- fit_object$fixed_features
  decision <- fit_object$decision
  x_train <- fit_object$x_train
  model_list <- fit_object$model_list
  autoregressive_model <- fit_object$autoregressive_model
  c_int <- fit_object$c_int

  time_process_start = Sys.time()

  n_explain <- nrow(x_explain)

#  mutable_features <- c(mutable_features,fixed_features[-29])

  mutable_features_plus <- c("id_explain",mutable_features)

  x_sim_mutable <- x_sim[,mutable_features_plus,with=F]
  x_explain_mutable <- cbind(id_explain=seq_len(n_explain),x_explain[,mutable_features,with=F])

  x_sim[,row_id:=1:.N]

  res.dt <- data.table::data.table(row_id=seq_len(nrow(x_sim)),id_explain=x_sim[,id_explain])


  get_measure_validation(res.dt,x_explain_mutable,x_sim_mutable,pred_sim,c_int)

  get_measure_sparsity(res.dt,x_explain_mutable,x_sim_mutable)
  get_measure_manhattan(res.dt,x_explain_mutable,x_sim_mutable)
  get_measure_euclidean(res.dt,x_explain_mutable,x_sim_mutable)

  data.table::setorder(res.dt,id_explain,-measure_validation,measure_sparsity,measure_manhattan,measure_euclidean)

  res.dt[, counterfactual_rank := 1:.N, by = id_explain]


  if(remove_invalid){
    res.dt <- res.dt[measure_validation==1]
  }

  ret_sim0 <- res.dt[,head(.SD,return_best_k),by=id_explain][,.(row_id,counterfactual_rank)]

  ret_sim <- x_sim[ret_sim0,on="row_id"]
  ret_sim[,row_id:=NULL]

  time_process = difftime(Sys.time(), time_process_start, units = "secs")

  data.table::setcolorder(ret_sim,c("id_explain","counterfactual_rank"))

  ret <- list(cf=ret_sim,
              time_process = time_process)

  return(ret)
}


get_measure_validation <- function(res.dt,x_explain_mutable,x_sim_mutable,pred_sim,c_int){
  value = (pred_sim>=c_int[1] & pred_sim<=c_int[2])*1
  res.dt[,measure_validation:= value]
}


get_measure_sparsity <- function(res.dt,x_explain_mutable,x_sim_mutable){
  n_explain <- x_explain_mutable[,.N]
  for (i in seq_len(n_explain)){
    value <- Rfast::colsums(unlist(x_explain_mutable[id_explain==i,-1])-t(as.matrix(x_sim_mutable[id_explain==i,-1]))==0,parallel=T)
    res.dt[id_explain==i,measure_sparsity:=value]
  }
}
get_measure_manhattan <- function(res.dt,x_explain_mutable,x_sim_mutable){
  n_explain <- x_explain_mutable[,.N]
  for (i in seq_len(n_explain)){
    value <- Rfast::dista(x_explain_mutable[id_explain==i,-1],x_sim_mutable[id_explain==i,-1],trans=F,type = "manhattan")
    res.dt[id_explain==i,measure_manhattan:=value]
    #set(res.dt,i=which(res.dt$id_explain==i),j = "measure_manhattan",value = Rfast::dista(x_explain_mutable[id_explain==i,-1],x_sim_mutable[id_explain==i,-1],trans=F,type = "manhattan"))
  }
}

get_measure_euclidean <- function(res.dt,x_explain_mutable,x_sim_mutable){
  n_explain <- x_explain_mutable[,.N]
  for (i in seq_len(n_explain)){
    value <- Rfast::dista(x_explain_mutable[id_explain==i,-1],x_sim_mutable[id_explain==i,-1],trans=F)
    res.dt[id_explain==i,measure_euclidean:=value]
#    set(res.dt,i=which(res.dt$id_explain==i),j="measure_euclidean",value = Rfast::dista(x_explain_mutable[id_explain==i,-1],x_sim_mutable[id_explain==i,-1],trans=F))
  }
}


