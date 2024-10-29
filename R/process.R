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
process <- function(x_sim,
                    pred_sim,
                    x_explain,
                    fit_object,
                    measures = c("validation", "L0", "L1"), # Don't obey this quite yet
                    remove_invalid = TRUE,
                    return_best_k = 1,
                    sort_by_measures_order = TRUE) { # Don't obey this quite yet

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

  time_process_start <- Sys.time()

  n_explain <- nrow(x_explain)


  mutable_features_plus <- c("id_explain", mutable_features)

  x_sim_mutable <- x_sim[, mutable_features_plus, with = F]
  x_explain_mutable <- cbind(id_explain = seq_len(n_explain), x_explain[, mutable_features, with = F])

  x_sim[, row_id := 1:.N]

  res.dt <- data.table::data.table(row_id = seq_len(nrow(x_sim)), id_explain = x_sim[, id_explain], pred = pred_sim)

  measure_ordering <- c()
  for (this_measure in measures) {
    if (this_measure == "validation") {
      get_measure_validation(res.dt, x_explain_mutable, x_sim_mutable, c_int)
      measure_ordering <- c(measure_ordering, -1)
    }

    if (this_measure == "L0") {
      get_measure_L0(res.dt, x_explain_mutable, x_sim_mutable)
      measure_ordering <- c(measure_ordering, 1)
    }
    if (this_measure == "L1") {
      get_measure_L1(res.dt, x_explain_mutable, x_sim_mutable)
      measure_ordering <- c(measure_ordering, 1)
    }
    if (this_measure == "L2") {
      get_measure_L2(res.dt, x_explain_mutable, x_sim_mutable)
      measure_ordering <- c(measure_ordering, 1)
    }
    if (this_measure == "gower") {
      get_measure_gower(res.dt, x_explain_mutable, x_sim_mutable)
      measure_ordering <- c(measure_ordering, 1)
    }
  }


  data.table::setorderv(res.dt, cols = c("id_explain", paste0("measure_", measures)), order = c(1, measure_ordering))

  res.dt[, counterfactual_rank := 1:.N, by = id_explain]


  if (remove_invalid) {
    res.dt <- res.dt[measure_validation == 1]
  }

  cols <- c("counterfactual_rank", "row_id", "pred", paste0("measure_", measures))

  ret_sim0 <- res.dt[, head(.SD, return_best_k), by = id_explain][, cols, with = FALSE]

  ret_sim <- x_sim[ret_sim0, on = "row_id"]

  time_process <- difftime(Sys.time(), time_process_start, units = "secs")


  data.table::setcolorder(ret_sim, c("id_explain", cols[-2]))

  cols_measure <- c("id_explain", cols[-2])
  cols_cf <- names(ret_sim)[!(names(ret_sim) %in% cols[-1])]

  ret <- list(
    cf = ret_sim[, cols_cf, with = FALSE],
    cf_measures = ret_sim[, cols_measure, with = FALSE],
    time_process = time_process
  )

  return(ret)
}


get_measure_validation <- function(res.dt, x_explain_mutable, x_sim_mutable, c_int) {
  res.dt[, measure_validation := (pred >= c_int[1] & pred <= c_int[2]) * 1]
}


get_measure_L0 <- function(res.dt, x_explain_mutable, x_sim_mutable) {
  n_features <- ncol(x_explain_mutable) - 1

  combined <- x_explain_mutable[x_sim_mutable, on = "id_explain", nomatch = 0]

  # Identify columns to compare (exclude `id_explain`)
  columns_to_compare <- setdiff(names(x_explain_mutable), "id_explain")

  value <- identical_rows(combined, columns_to_compare)

  res.dt[, measure_L0 := n_features - value] # Number of features changed from original value
}

identical_rows <- function(combined, columns_to_compare) {
  # Calculate the number of identical values in each row
  value <- combined[, rowSums(mapply(
    function(col1, col2) col1 == col2,
    .SD[, columns_to_compare, with = FALSE],
    .SD[, paste0("i.", columns_to_compare), with = FALSE]
  ))]
}



get_measure_L1 <- function(res.dt, x_explain_mutable, x_sim_mutable) {
  n_explain <- x_explain_mutable[, .N]
  for (i in seq_len(n_explain)) {
    value <- Rfast::dista(x_explain_mutable[id_explain == i, -1], x_sim_mutable[id_explain == i, -1], trans = F, type = "manhattan")
    res.dt[id_explain == i, measure_L1 := value]
    # set(res.dt,i=which(res.dt$id_explain==i),j = "measure_manhattan",value = Rfast::dista(x_explain_mutable[id_explain==i,-1],x_sim_mutable[id_explain==i,-1],trans=F,type = "manhattan"))
  }
}

get_measure_L2 <- function(res.dt, x_explain_mutable, x_sim_mutable) {
  n_explain <- x_explain_mutable[, .N]
  for (i in seq_len(n_explain)) {
    value <- Rfast::dista(x_explain_mutable[id_explain == i, -1], x_sim_mutable[id_explain == i, -1], trans = F)
    res.dt[id_explain == i, measure_L2 := value]
    #    set(res.dt,i=which(res.dt$id_explain==i),j="measure_euclidean",value = Rfast::dista(x_explain_mutable[id_explain==i,-1],x_sim_mutable[id_explain==i,-1],trans=F))
  }
}


get_measure_gower <- function(res.dt, x_explain_mutable, x_sim_mutable) {
  cat_cols <- names(which(sapply(x_explain_mutable[, -1], is.factor)))
  num_cols <- names(which(sapply(x_explain_mutable[, -1], is.numeric)))

  combined <- x_explain_mutable[x_sim_mutable, on = "id_explain", nomatch = 0]

  if (length(cat_cols) > 0) {
    cat_contrib <- identical_rows(combined, cat_cols)
  } else {
    cat_contrib <- 0
  }
  num_contrib <- 0
  if (length(num_cols) > 0) {
    for (i in seq_along(num_cols)) {
      col1 <- unlist(combined[, num_cols[i], with = FALSE])
      col2 <- unlist(combined[, paste0("i.", num_cols[i]), with = FALSE])

      num_contrib <- num_contrib + abs(col1 - col2) / diff(range(col2)) # Using the range of the syntehtic data instead of the original for simplicity
    }
  }
  res.dt[, measure_gower := cat_contrib + num_contrib]
}
