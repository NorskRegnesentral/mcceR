#' Generate potential counterfactuals from fitted autoregressive model
#'
#' @param fit_object List.
#' The output from calling the fit() function.
#'
#' @param K Numeric.
#' Number of potential counterfactuals to generate per row in x_explain.
#'
#' @param seed Positive integer.
#' Specifies the seed used when generating the potential counterfactuals.
#' If `NULL` the seed will be inherited from the calling environment.
#'
#' @inheritParams explain_mcce
#'
#' @return List. The generated potential counterfactuals and the timing of this function.
#'
#' @export
#'
generate = function(x_explain, fit_object, K = 1000, seed=NULL){



  mutable_features <- fit_object$mutable_features
  fixed_features <- fit_object$fixed_features
  decision <- fit_object$decision
  x_train <- fit_object$x_train
  model_list <- fit_object$model_list
  autoregressive_model <- fit_object$autoregressive_model
  n_explain <- nrow(x_explain)

  if (!is.matrix(x_explain) && !is.data.frame(x_explain)) {
    stop("x_explain should be a matrix or a data.frame/data.table.\n")
  } else {
    x_explain <- data.table::as.data.table(x_explain)
  }


  if(decision){
    set(x_explain, i = NULL, j="decision", value=1)
  }

  if(length(fixed_features)==0){
    x_explain[,dummy:=1]

    dup_features <- c("dummy",fixed_features)

  } else {
    dup_features <- fixed_features
  }


  explain_vec <- rep(seq_len(n_explain), each = K)

  time_generate_start = Sys.time()

  simData <- x_explain[explain_vec, dup_features,with=F]
  simData <- data.table::setDT(simData)

  rowno <- seq_len(nrow(x_train))
  N_mutable <- length(mutable_features)

  if(autoregressive_model=="ctree"){
    predict_node = predict_node.ctree
  } else if(autoregressive_model=="rpart"){
    predict_node = predict_node.rpart
  }

  if(!is.null(seed))set.seed(seed)
  for(i in seq_len(N_mutable)){
    this_feature <- mutable_features[i]

    fit.nodes <- predict_node(model = model_list[[i]])
    pred.nodes <- predict_node(model = model_list[[i]], newdata = simData)
    for (pred_node in unique(pred.nodes)){
      these_rows <- which(pred.nodes == pred_node)

      newrowno <- sample(rowno[fit.nodes == pred_node], length(these_rows),replace = TRUE)
      tmp <- unlist(x_train[newrowno, this_feature, with=F])
      data.table::set(simData,i = these_rows,j=this_feature, value = tmp)
    }
  }

  data.table::setcolorder(simData, neworder = names(x_train))
  data.table::set(simData,j="id_explain", value = explain_vec)

  if(length(fixed_features)==0){
    simData[,dummy:=NULL]
  }

  if(decision){
    data.table::set(simData, i = NULL, j="decision", value=NULL)
  }
  data.table::setcolorder(simData,"id_explain")

  time_generate = difftime(Sys.time(), time_generate_start, units = "secs")

  ret <- list(simData = simData,
              time_generate = time_generate)

  return(ret)
}

predict_node.ctree <- function(model,newdata=NULL){
  party::where(object=model,newdata=newdata)
}

predict_node.rpart <- function(model,newdata=NULL,version = "new"){
  if(version=="new"){
    # Using a modified version of rpart_leaves from the last answer here:  https://stackoverflow.com/questions/17597739/get-id-name-of-rpart-model-nodes
    # with getFromNamespace instead of ::: to avoid CRAN notes
    if(is.null(newdata)){
      return(model$where)
    }
    if (is.null(attr(newdata, "terms"))) {
      Terms <- delete.response(model$terms)
      newdata <- model.frame(Terms, newdata, na.action = na.pass,
                             xlev = attr(model, "xlevels"))
      if (!is.null(cl <- attr(Terms, "dataClasses")))
        .checkMFClasses(cl, newdata, TRUE)
    }
    newdata <- getFromNamespace("rpart.matrix", ns = "rpart")(newdata)
    where <- unname(getFromNamespace("pred.rpart", ns = "rpart")(model, newdata))
    return(where)
  } else {
    # Using partykit::as.party(model)
    # See: "https://stackoverflow.com/questions/36748531/getting-the-observations-in-a-rparts-node-i-e-cart"
    where <- predict(object=partykit::as.party(model),newdata=newdata,type="node")
    return(where)
  }
}


