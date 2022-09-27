.onLoad <- function(libname = find.package("mcceR"), pkgname = "mcceR") {

  # CRAN Note avoidance
  utils::globalVariables(
    c(
      ".",
      ".N",
      ".I",
      ".GRP",
      ".SD"
    )
  )
  invisible()
}
