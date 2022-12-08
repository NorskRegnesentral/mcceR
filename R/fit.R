#' Fit autoregressive model to the training data
#'
#' @param autoregressive_model Character.
#' Specifies the name of the autoregressive model used to fit the data.
#'
#' @param pred_train Numeric vector.
#' The prediction score for the data in x_train.
#' @param decision Logical.
#' Whether to include the decision threshold as a binary features to improve the efficiency of MCCE.
#'
#' @param seed Positive integer.
#' Specifies the seed used when fitting the autoregressive feature dependence model.
#' If `NULL` the seed will be inherited from the calling environment.
#'
#' @inheritParams explain_mcce
#'
#' @return List. Includes model_list with the fitted models
#'
#' @export
#'
fit = function(x_train, pred_train, fixed_features, c_int=c(mean(pred_train),1), autoregressive_model="ctree", seed=NULL,decision = TRUE){

  if(!is.null(seed)) set.seed(seed)

  if (!is.matrix(x_train) && !is.data.frame(x_train)) {
    stop("x_train should be a matrix or a data.frame/data.table.\n")
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

  if(decision){
    decision0 <- (pred_train>=c_int[1] & pred_train<=c_int[2])*1

    #  x_train[,decision := decision0]
    data.table::set(x_train, i = NULL, j="decision", value=decision0)
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
#    print(i)
  }

  time_fit = difftime(Sys.time(), time_fit_start, units = "secs")
  ret <- list(model_list = model_list,
              time_fit = time_fit,
              fixed_features = fixed_features,
              mutable_features = mutable_features,
              autoregressive_model = autoregressive_model,
              decision = decision,
              x_train = x_train,
              c_int = c_int)

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
