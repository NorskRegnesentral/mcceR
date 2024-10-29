#' Generate predictions for input data with specified model
#'
#' @description Performs prediction of response
#' [stats::lm()],
#' [stats::glm()],
#' [ranger::ranger()],
#' [mgcv::gam()] and
#' [xgboost::xgb.train()] with binary or continuous
#' response. See details for more information.
#'
#' NOTE: You should never need to call this function explicitly.
#' It is exported just to be easier accessible for users, see details.
#'
#' @param x Model object for the model to be explained.
#' @param newdata A data.frame/data.table with the features to predict from.
#'
#' @details The following models are currently supported:
#' \itemize{
#' \item [stats::lm()]
#' \item [stats::glm()]
#' \item [ranger::ranger()]
#' \item [mgcv::gam()]
#' \item [xgboost::xgb.train()]
#' }
#'
#' If you have a binary classification model we'll always return the probability prediction
#' for a single class.
#'
#' If you are explaining a model not supported natively, you need to create the `[predict_model()]` function yourself,
#' and pass it on to as an argument to `[explain()]`.
#'
#' @return Numeric. Vector of size equal to the number of rows in `newdata`.
#'
#' @export
#' @keywords internal
predict_model <- function(x, newdata) {
  UseMethod("predict_model", x)
}

#' @rdname predict_model
#' @export
predict_model.default <- function(x, newdata) {
  str_error <- paste(
    "It seems that you passed a non-valid model object.",
    "See more information about which models that are supported",
    "by running ?predict_model."
  )
  stop(str_error)
}


#' @rdname predict_model
#' @export
predict_model.xgb.Booster <- function(x, newdata) {
  if (!requireNamespace("stats", quietly = TRUE)) {
    stop("The xgboost package is required for predicting xgboost models")
  }

  predict(x, as.matrix(newdata))
}

#' @rdname predict_model
#' @export
predict_model.ranger <- function(x, newdata) {
  if (!requireNamespace("ranger", quietly = TRUE)) {
    stop("The ranger package is required for predicting ranger models")
  }

  if (x$treetype == "Probability estimation") {
    predict(x, newdata)$predictions[, 2]
  } else {
    predict(x, newdata)$predictions
  }
}

#' @rdname predict_model
#' @export
predict_model.lm <- function(x, newdata) {
  if (!requireNamespace("stats", quietly = TRUE)) {
    stop("The stats package is required for predicting stats models")
  }

  predict(x, as.data.frame(newdata))
}

#' @rdname predict_model
#' @export
predict_model.gam <- function(x, newdata) {
  if (!requireNamespace("mgcv", quietly = TRUE)) {
    stop("The mgcv package is required for predicting gam models")
  }

  if (x$family[[1]] == "binomial") {
    as.vector(
      predict(x, as.data.frame(newdata), type = "response")
    )
  } else {
    as.vector(
      predict(x, as.data.frame(newdata))
    )
  }
}

#' @rdname predict_model
#' @export
predict_model.glm <- function(x, newdata) {
  if (!requireNamespace("stats", quietly = TRUE)) {
    stop("The stats package is required for predicting stats models")
  }

  if (x$family[[1]] == "binomial") {
    predict(x, as.data.frame(newdata), type = "response")
  } else {
    predict(x, as.data.frame(newdata))
  }
}


#' Model testing function
#'
#' @keywords internal
get_predict_model <- function(predict_model, model) {
  # Checks that predict_model is a proper function (R + py)
  # Extracts natively supported functions for predict_model if exists and not passed (R only)
  # Checks that predict_model provide the right output format (R and py)
  # Returns the predict_model to use subsequently (R only)

  model_class0 <- class(model)[1]

  # checks predict_model
  if (!(is.function(predict_model)) &&
    !(is.null(predict_model))) {
    stop("`predict_model` must be NULL or a function.")
  }

  supported_models <- substring(rownames(attr(methods(predict_model), "info")), first = 15)

  # Get native predict_model if not passed and exists
  if (is.null(predict_model)) {
    if (model_class0 %in% supported_models) {
      predict_model <- mcceR::predict_model
    } else {
      stop(
        "You passed a model to explain_mcce() which is not natively supported, and did not supply the 'predict_model' ",
        "function to explain_mcce().\n",
      )
    }
  }


  return(predict_model)
}
