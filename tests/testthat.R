if (requireNamespace("testthat", quietly = TRUE)) {
  library("testthat")
  library("checkmate")
  library("mlr3")
  library("mlr3pipelines")
  library("mlr3fairness")
  test_check("mlr3fairness")
}
