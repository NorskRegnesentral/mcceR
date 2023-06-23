#' Fit autoregressive model to the training data
#'
#' @param model The model whose predictions we want to explain.
#'
#' @param x_explain Matrix or data.frame/data.table.
#' Contains the data that we want to generate counterfactuals for.
#'
#' @param x_train Matrix or data.frame/data.table.
#' Contains the data used to train the autoregressive feature dependence model.
#'
#' @param predict_model Function.
#' The prediction function.
#' The function must have two arguments, `model` and `newdata` which specify, respectively, the model
#' and a data.frame/data.table to compute predictions for. The function must give the prediction as a numeric vector.
#'
#' @param fixed_features Character vector.
#' Names of features to fix in counterfactual generation.
#'
#' @param c_int Numeric vector (length 2).
#'  Specifies the range of prediction values corresponding to the desired decision
#'
#' @param fit.autoregressive_model Character.
#' Specifies the name of the autoregressive model used to fit the data.
#'
#' @param fit.decision Logical.
#' Whether to include the decision threshold as a binary features to improve the efficiency of MCCE.
#'
#' @param fit.seed Positive integer.
#' Specifies the seed used when fitting the autoregressive feature dependence model.
#' If `NULL` the seed will be inherited from the calling environment.
#'
#' @param generate.K Numeric.
#' Number of potential counterfactuals to generate per row in x_explain.
#'
#' @param generate.seed Positive integer.
#' Specifies the seed used when generating the potential counterfactuals.
#' If `NULL` the seed will be inherited from the calling environment.
#'
#' @param process.measures Character vector.
#' Indicates the measures (in the given order) that are applied to the simulate data.
#'
#' @param process.return_best_k Integer.
#' How many counterfactuals should be generated per prediction to explain (at max).
#'
#' @param process.remove_invalid Logical.
#' Indicates whether invalid counterfactuals should be removed from the returned counterfactual list.
#'
#' @param process.sort_by_measures_order Logical.
#' Indicates whether the counterfactuals should be sorted.
#'
#' @param store_model_list Logical.
#' Indicates whether the list of models used to fit the feature distribution should be stored and returned to the user.
#'
#' @param store_sim_data Logical.
#' Indicates whether the simulated data (before any pre-processing) should be stored and returned to the user.
#'
#' @param timing Logical.
#' Whether the timing of the different parts of the `explain()` should saved in the model object.
#'
#' @param ... Additional arguments passed to \code{\link[party:ctree]{party::ctree()}} or
#' \code{\link[rpart:rpart]{rpart::rpart()}}
#'
#' @return List. The counterfactuals for the predictions we explain + various supporting info
#'
#' @export
#'
explain_mcce = function(model, x_explain, x_train, predict_model=NULL,
                        fixed_features = NULL, c_int=c(0.5,1),
                        fit.autoregressive_model="ctree", fit.decision = TRUE, fit.seed = NULL,
                        generate.K = 1000, generate.seed = NULL,
                        process.measures = c("validation","L0","L1"),
                        process.return_best_k = 1,
                        process.remove_invalid = TRUE,
                        process.sort_by_measures_order = TRUE,
                        store_model_list = FALSE,
                        store_sim_data = FALSE,
                        timing = TRUE,
                        ...){

  if (!is.matrix(x_train) && !is.data.frame(x_train)) {
    stop("x_train should be a matrix or a data.frame/data.table.\n")
  } else {
    x_train <- data.table::as.data.table(x_train)
  }

  if (!is.matrix(x_explain) && !is.data.frame(x_explain)) {
    stop("x_explain should be a matrix or a data.frame/data.table.\n")
  } else {
    x_explain <- data.table::as.data.table(x_explain)
  }

  predict_model <- get_predict_model(predict_model, model)


  pred_train <- predict_model(model,x_train)

  fit_object <- fit(x_train = x_train,
                    pred_train = pred_train,
                    fixed_features = fixed_features,
                    c_int = c_int,
                    autoregressive_model = fit.autoregressive_model,
                    decision = fit.decision,
                    seed = fit.seed,
                    ...)



  sim_object <- generate(x_explain = x_explain,
                         fit_object=fit_object,
                         K = generate.K,
                         seed=generate.seed)

  x_sim <- sim_object$simData

  pred_sim <- predict_model(model,x_sim[,-"id_explain"])

  cfs <- process(x_sim = x_sim,
                 pred_sim = pred_sim,
                 x_explain = x_explain,
                 fit_object = fit_object,
                 measures = process.measures, # Don't obey this quite yet
                 remove_invalid = process.remove_invalid,
                 return_best_k = process.return_best_k,
                 sort_by_measures_order = process.sort_by_measures_order)

  time_vec <- c(fit.time=fit_object$time_fit,
                generate.time=sim_object$time_generate,
                process.time=cfs$time_process)

  ret <- list(cf = cfs$cf[],
              fixed_features = fit_object$fixed_features,
              mutable_features = fit_object$mutable_features,
              time = time_vec)

  if(store_model_list==TRUE){
    ret$model_list <- fit_object$model_list
  }
  if(store_sim_data==TRUE){
    ret$sim_data <- x_sim
  }
  if (timing == FALSE) {
    ret$time <- NULL
  }


  attr(ret, "class") <- c("mcce", "list")

  return(ret)
}
