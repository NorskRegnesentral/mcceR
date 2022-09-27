#' Fit autoregressive model to the training data
#'
#' @param x_train Data.frame or data.table of features
#'
#' @param pred_train Numeric vector. Specifies the prediction value for the observations in x_train.
#'
#' @param fixed_features Character vector. Names of features to fix in counterfactual generation.
#'
#' @param c_val Numeric. Specifying the decision threshold to exceed to achieve the right decison
#'
#' @param autoregressive_model Character. Specifies the name of the autoregressive model used to fit the data.
#'
#' @param include_decision Logical. Whether to include the decision threshold as a binary features to improve the efficiency of MCCE.
#'
#' @param seed Numeric.
#'
#' @return List. Includes model_list with the fitted models
#'
#' @export
#'
fit = function(x_train, pred_train, fixed_features, c_val=mean(pred_train), autoregressive_model="ctree", seed=NULL,include_decision = TRUE){

  if(!is.null(seed)) set.seed(seed)

  if (!is.matrix(x_train) && !is.data.frame(x_train)) {
    stop(paste0(stop_message,"x_train should be a matrix or a data.frame/data.table.\n"))
  } else {
    x_train <- data.table::as.data.table(x_train)
  }

  if (!all(fixed_features %in% names(x_train))) {
    missing_feature <- fixed_features[!(fixed_features %in% names(x_train))]
    stop(
      paste0(
        "The fixed feature(s) ", paste0(missing_feature, collapse = ", "), " are not\n",
        "among the features specified by the x_train."
      )
    )
  }

  if(include_decision){
    decision0 <- (pred_train>=c_val)*1

    #  x_train[,decision := decision0]
    set(x_train, i = NULL, j="decision", value=decision0)
  }

  fixed_features <- c(fixed_features,"decision")

  mutable_features <- names(x_train)[!(names(x_train) %in% fixed_features)]

  N_fixed = length(fixed_features)
  N_mutable = length(mutable_features)

  current_x <- fixed_features
  model_list <- list()

  time_fit_start = Sys.time()
  # Fit the models
  for(i in seq_len(N_mutable)){

    response <- mutable_features[i]
    features <- current_x
    if(autoregressive_model=="ctree"){
      model_list[[i]] <- model.ctree(response,features,data=x_train)
    } else if(autoregressive_model=="rpart"){
      model_list[[i]] <- model.rpart(response,features,data=x_train)
    } else {
      stop("autoregressive_model argument not recognized.")
    }

    current_x <- c(current_x, mutable_features[i])
    print(i)
  }

  time_fit = difftime(Sys.time(), time_fit_start, units = "secs")
  ret <- list(model_list = model_list,
              time_fit = time_fit,
              fixed_features = fixed_features,
              mutable_features = mutable_features,
              autoregressive_model = autoregressive_model,
              include_decision = include_decision,
              x_train = x_train)

  return(ret)
}


model.ctree <- function(response,features,data){
  formula <- as.formula(paste0(response, "~", paste0(features, collapse = "+")))
  mod <- party::ctree(formula = formula,
                      data = data)

}

model.rpart <- function(response,features,data){
  formula <- as.formula(paste0(response, "~", paste0(features, collapse = "+")))
  mod <- rpart::rpart(formula = formula,
                      data = data)

}
