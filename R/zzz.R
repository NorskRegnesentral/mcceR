.onLoad <- function(libname = find.package("mcceR"), pkgname = "mcceR") {

  # CRAN Note avoidance
  utils::globalVariables(
    c(
      ".",
      ".N",
      ".I",
      ".GRP",
      ".SD",
      "id_explain",
      "measure_euclidean",
      "measure_manhattan",
      "measure_validation",
      "measure_sparsity",
      "row_id",
      "counterfactual_rank",
      "get_measure_L0",
      "get_measure_L1",
      "get_measure_L2",
      "get_measure_validation",
      "get_predict_model",
      "predict_node.rpart",
      "pred"
    )
  )
  invisible()
}
