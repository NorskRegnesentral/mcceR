#' Fit and sample a set of ctrees based on mutable (not fixed) features
#'
#' @param x_train Data.frame or data.table of features and response
#'
#' @param pred_train Numeric vector. Specifies the prediction value for the observations in x_train.
#'
#' @param fixed_features Character vector. Names of features to fix in counterfactual generation.
#'
#' @param c_val Numeric. Specifying the decision threshold to exceed to achieve the right decison
#'
#' @param autoregressive_model Character. Specifies the name of the autoregressive model used to fit the data.
#'
#' @param seed Numeric.
#'
#' @return List. Includes \code{synData} of class "synds" which includes the counterfactual exaplanations.
#'
#' @export
#'
fit = function(x_train, pred_train, fixed_features, c_val=mean(pred_train), autoregressive_model="ctree", seed=NULL){

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

  decision0 <- (pred_train>=c_val)*1

#  x_train[,decision := decision0]
  set(x_train, i = NULL, j="decision", value=decision0)

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
              time_fit = time_fit)

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