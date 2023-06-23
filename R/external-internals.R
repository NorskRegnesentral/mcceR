
#' Copy of rpart:::rpart.matrix
#' @keywords internal
rpart_rpart_matrix <- function (frame)
{
  if (!inherits(frame, "data.frame") || is.null(attr(frame,
                                                     "terms")))
    return(as.matrix(frame))
  frame[] <- lapply(frame, function(x) {
    if (is.character(x))
      as.numeric(factor(x))
    else if (!is.numeric(x))
      as.numeric(x)
    else x
  })
  X <- model.matrix(attr(frame, "terms"), frame)[, -1L, drop = FALSE]
  colnames(X) <- sub("^`(.*)`", "\\1", colnames(X))
  class(X) <- c("rpart.matrix", class(X))
  X
}

#' Copy of rpart:::pred.rpart
#' @keywords internal
rpart_pred_rpart <- function (fit, x)
{
  frame <- fit$frame
  if (nrow(frame) == 1L)
    return(structure(rep(1, nrow(x), names = rownames(x))))
  nc <- frame[, c("ncompete", "nsurrogate")]
  frame$index <- 1L + c(0L, cumsum((frame$var != "<leaf>") +
                                     nc[[1L]] + nc[[2L]]))[-(nrow(frame) + 1L)]
  frame$index[frame$var == "<leaf>"] <- 0L
  vnum <- match(rownames(fit$split), colnames(x))
  if (any(is.na(vnum)))
    stop("Tree has variables not found in new data")
  temp <- .Call(C_pred_rpart, as.integer(dim(x)), as.integer(dim(frame)[1L]),
                as.integer(dim(fit$splits)), as.integer(if (is.null(fit$csplit)) rep(0L,
                                                                                     2L) else dim(fit$csplit)), as.integer(row.names(frame)),
                as.integer(unlist(frame[, c("n", "ncompete", "nsurrogate",
                                            "index")])), as.integer(vnum), as.double(fit$splits),
                as.integer(fit$csplit - 2L), as.integer((fit$control)$usesurrogate),
                as.double(x), as.integer(is.na(x)))
  names(temp) <- rownames(x)
  temp
}


rpart_delete_response <- function (termobj)
{
  a <- attributes(termobj)
  y <- a$response
  if (!is.null(y) && y) {
    termobj[[2L]] <- NULL
    a$response <- 0
    a$variables <- a$variables[-(1 + y)]
    a$predvars <- a$predvars[-(1 + y)]
    if (length(a$factors))
      a$factors <- a$factors[-y, , drop = FALSE]
    if (length(a$offset))
      a$offset <- ifelse(a$offset > y, a$offset - 1, a$offset)
    if (length(a$specials))
      for (i in seq_along(a$specials)) {
        b <- a$specials[[i]]
        a$specials[[i]] <- ifelse(b > y, b - 1, b)
      }
    attributes(termobj) <- a
  }
  termobj
}
