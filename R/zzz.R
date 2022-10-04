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
      "counterfactual_rank"
    )
  )
  invisible()
}
