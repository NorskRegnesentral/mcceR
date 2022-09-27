#' Generate potential counterfactuals from fitted autoregressive model
#'
#' @param x_explain Data.frame or data.table of features to generate counterfactuals for
#'
#' @param fit_object List. The output from calling the fit function.
#'
#' @param K Numeric. Number of potential counterfactuals to generate#'
#'
#' @param seed Numeric.
#'
#' @return data.table. The simulated data with a dec
#'
#' @export
#'
generate = function(x_explain, fit_object, K = 1000, seed=NULL){



  mutable_features <- fit_object$mutable_features
  fixed_features <- fit_object$fixed_features
  include_decision <- fit_object$include_decision
  x_train <- fit_object$x_train
  model_list <- fit_object$model_list
  autoregressive_model <- fit_object$autoregressive_model
  n_explain <- nrow(x_explain)

  if (!is.matrix(x_explain) && !is.data.frame(x_explain)) {
    stop(paste0(stop_message,"x_explain should be a matrix or a data.frame/data.table.\n"))
  } else {
    x_explain <- data.table::as.data.table(x_explain)
  }


  if(include_decision){
    set(x_explain, i = NULL, j="decision", value=1)
  }

  explain_vec <- rep(seq_len(n_explain), each = K)

  time_sample_start = Sys.time()

  simData <- x_explain[explain_vec, fixed_features,with=F]
  simData <- setDT(simData)

  rowno <- seq_len(nrow(x_train))
  N_mutable <- length(mutable_features)

  if(autoregressive_model=="ctree"){
    predict_node = predict_node.ctree
  } else if(autoregressive_model=="rpart"){
    # Using partykit::as.party(model)
    # See: "https://stackoverflow.com/questions/36748531/getting-the-observations-in-a-rparts-node-i-e-cart"
    predict_node = predict_node.rpart
  }

  if(!is.null(seed))set.seed(seed)
  for(i in seq_len(N_mutable)){
    this_feature <- mutable_features[i]

    fit.nodes <- predict_node(model=model_list[[i]])
    pred.nodes <- predict_node(model= model_list[[i]], newdata = simData)
    for (pred_node in unique(pred.nodes)){
      these_rows <- which(pred.nodes == pred_node)

      newrowno <- sample(rowno[fit.nodes == pred_node], length(these_rows),replace = TRUE)
      tmp <- unlist(x_train[newrowno, this_feature, with=F])
      set(simData,i = these_rows,j=this_feature, value = tmp)
    }
  }
  setcolorder(simData, neworder = names(x_train))
  set(simData,j="id_explain", value = explain_vec)

  if(include_decision){
    set(simData, i = NULL, j="decision", value=NULL)
  }
  setcolorder(simData,"id_explain")

  time_sample = difftime(Sys.time(), time_sample_start, units = "secs")

  ret <- list(simData = simData,
              time_sample = time_sample)

  return(ret)
}


predict_node.ctree <- function(model,newdata=NULL){
  partykit::where(object=model,newdata=newdata)
}

predict_node.rpart <- function(model,newdata=NULL){
  predict(object=partykit::as.party(model),newdata=newdata,type="node")
}

