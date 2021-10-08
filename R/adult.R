#' @title Adult Dataset
#'
#' @name adult
#' @aliases adult_test
#' @aliases adult_train
#'
#' @description
#' Dataset used to predict whether income exceeds $50K/yr based on census data.
#' Also known as "Census Income" dataset
#' Train dataset contains 13 features and 30178 observations.
#' Test dataset contains 13 features and 15315 observations.
#' Target column is "target": A binary factor where 1: <=50K and 2: >50K for annual income.
#' The column `"sex"` is set as protected attribute.
#'
#' @section Pre-processing:
#' * `country` column has been removed since we expect them have no predictive power.
#' * `fnlwgt` column has been removed since we expect them have no predictive power.
#' * Rows containing `NA` in workclass and occupation have been removed.
#'
#' @source  @misc{Dua:2019 ,
#'   author = "Dua, Dheeru and Graff, Casey",
#'   year = "2017",
#'   title = "{UCI} Machine Learning Repository",
#'   url = "http://archive.ics.uci.edu/ml",
#'   institution = "University of California, Irvine, School of Information and Computer Sciences"
#'  }
#'
#' @docType data
#' @keywords data
#' @examples
#' data("adult_test", package = "mlr3fairness")
#' data("adult_train", package = "mlr3fairness")
NULL

get_adult_task_train = function() { # nocov start
  b = as_backend("adult_train")
  task = mlr3::TaskClassif$new("adult_train", b, target = "target")
  task$col_roles$pta = "sex"
  b$hash = task$man = "mlr3fairness::mlr_tasks_adult_train"
  task
}  # nocov end

get_adult_task_test = function() {  # nocov start
  b = as_backend("adult_test")
  task = mlr3::TaskClassif$new("adult_test", b, target = "target")
  task$col_roles$pta = "sex"
  b$hash = task$man = "mlr3fairness::mlr_tasks_adult_test"
  task
} # nocov end
