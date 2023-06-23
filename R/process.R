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

  if (!is.matrix(x_sim) && !is.data.frame(x_sim)) {
    stop("x_sim should be a matrix or a data.frame/data.table.\n")
  } else {
    x_sim <- data.table::as.data.table(x_sim)
  }

  if (!is.matrix(x_explain) && !is.data.frame(x_explain)) {
    stop("x_explain should be a matrix or a data.frame/data.table.\n")
  } else {
    x_explain <- data.table::as.data.table(x_explain)
  }


  mutable_features <- fit_object$mutable_features
  c_int <- fit_object$c_int

  time_process_start = Sys.time()

  n_explain <- nrow(x_explain)


  mutable_features_plus <- c("id_explain",mutable_features)

  x_sim_mutable <- x_sim[,mutable_features_plus,with=F]
  x_explain_mutable <- cbind(id_explain=seq_len(n_explain),x_explain[,mutable_features,with=F])

  x_sim[,row_id:=1:.N]

  res.dt <- data.table::data.table(row_id=seq_len(nrow(x_sim)),id_explain=x_sim[,id_explain],pred=pred_sim)

  measure_ordering <- c()
  for(this_measure in measures){

    if(this_measure=="validation"){
      get_measure_validation(res.dt,x_explain_mutable,x_sim_mutable,c_int)
      measure_ordering <- c(measure_ordering,-1)
    }

    if(this_measure=="L0"){
      get_measure_L0(res.dt,x_explain_mutable,x_sim_mutable)
      measure_ordering <- c(measure_ordering,1)
    }
    if(this_measure=="L1"){
      get_measure_L1(res.dt,x_explain_mutable,x_sim_mutable)
      measure_ordering <- c(measure_ordering,1)
    }
    if(this_measure=="L2"){
      get_measure_L2(res.dt,x_explain_mutable,x_sim_mutable)
      measure_ordering <- c(measure_ordering,1)
    }

  }


  data.table::setorderv(res.dt,cols=c("id_explain",paste0("measure_",measures)),order = c(1,measure_ordering))

  res.dt[, counterfactual_rank := 1:.N, by = id_explain]


  if(remove_invalid){
    res.dt <- res.dt[measure_validation==1]
  }

  ret_sim0 <- res.dt[,head(.SD,return_best_k),by=id_explain][,.(row_id,counterfactual_rank,pred)]

  ret_sim <- x_sim[ret_sim0,on="row_id"]
  ret_sim[,row_id:=NULL]

  time_process = difftime(Sys.time(), time_process_start, units = "secs")

  data.table::setcolorder(ret_sim,c("id_explain","counterfactual_rank","pred"))

  ret <- list(cf=ret_sim,
              time_process = time_process)

  return(ret)
}


get_measure_validation <- function(res.dt,x_explain_mutable,x_sim_mutable,c_int){
  res.dt[,measure_validation:= (pred>=c_int[1] & pred<=c_int[2])*1]
}


get_measure_L0 <- function(res.dt,x_explain_mutable,x_sim_mutable){
  n_explain <- x_explain_mutable[,.N]
  for (i in seq_len(n_explain)){
    value <- Rfast::colsums(unlist(x_explain_mutable[id_explain==i,-1])-t(as.matrix(x_sim_mutable[id_explain==i,-1]))==0,parallel=T)
    res.dt[id_explain==i,measure_L0:=value]
  }
}
get_measure_L1 <- function(res.dt,x_explain_mutable,x_sim_mutable){
  n_explain <- x_explain_mutable[,.N]
  for (i in seq_len(n_explain)){
    value <- Rfast::dista(x_explain_mutable[id_explain==i,-1],x_sim_mutable[id_explain==i,-1],trans=F,type = "manhattan")
    res.dt[id_explain==i,measure_L1:=value]
    #set(res.dt,i=which(res.dt$id_explain==i),j = "measure_manhattan",value = Rfast::dista(x_explain_mutable[id_explain==i,-1],x_sim_mutable[id_explain==i,-1],trans=F,type = "manhattan"))
  }
}

get_measure_L2 <- function(res.dt,x_explain_mutable,x_sim_mutable){
  n_explain <- x_explain_mutable[,.N]
  for (i in seq_len(n_explain)){
    value <- Rfast::dista(x_explain_mutable[id_explain==i,-1],x_sim_mutable[id_explain==i,-1],trans=F)
    res.dt[id_explain==i,measure_L2:=value]
#    set(res.dt,i=which(res.dt$id_explain==i),j="measure_euclidean",value = Rfast::dista(x_explain_mutable[id_explain==i,-1],x_sim_mutable[id_explain==i,-1],trans=F))
  }
}


