#' @title Adult Dataset
#'
#' @name adult
#' @aliases adult_dataset
#'
#' @description
#' Dataset used to predict whether income exceeds $50K/yr based on census data. Also known as "Census Income" dataset
#' Train dataset contains 13 features and 30178 observations.
#' Test dataset contains 13 features and 15315 observations.
#' Target column is "Target".
#'
#' @section Pre-processing:
#' * Country column has been removed since we expect them have no predictive power.
#' * fnlwgt column has been removed since we expect them have no predictive power.
#' * Rows contain NA in Workclass and Occupation has been removed.
#'
#' @source \url{https://archive.ics.uci.edu/ml/datasets/adult}
#'
#' @docType data
#' @keywords data
#' @examples
#' data("adult", package = "mlr3fairness")
#'


get_adult_task = function() {
  #Not sure if this need to be implemented. For now only add the documents
}
