#' @import mlr3
#' @import mlr3misc
#' @import mlr3verse
#' @import R6
#' @import checkmate
#' @import mlr3measures
#' @importFrom utils getFromNamespace data
"_PACKAGE"

.onLoad = function(libname, pkgname) { # nolint
  # register tasks
  x = getFromNamespace("mlr_tasks", ns = "mlr3")
  x$add("adult_train", get_adult_task_train())
  x$add("adult_test", get_adult_task_test())
  x$add("compas", get_compas_task())

  # teach mlr3 about the new column role "pta" (protected attribute)
  x = getFromNamespace("mlr_reflections", ns = "mlr3")
  x$task_col_roles$classif = union(x$task_col_roles$classif, "pta")
  x$task_col_roles$regr = union(x$task_col_roles$regr, "pta")
  # TODO: also do this for survival task in mlr3proba

  x = getFromNamespace("mlr_measures", ns = "mlr3")
  x$add("fairness", MeasureFairness)

  # Define a set of widely used metrics. Documented in mlr_measures_fairness
  x$add("fairness.fpr", MeasureFairness, base_measure = msr("classif.fpr"), operation = "groupwise_quotient")
  x$add("fairness.fnr", MeasureFairness, base_measure = msr("classif.fnr"), operation = "groupwise_quotient")
  x$add("fairness.tpr", MeasureFairness, base_measure = msr("classif.tpr"), operation = "groupwise_quotient")
  x$add("fairness.ppv", MeasureFairness, base_measure = msr("classif.ppv"), operation = "groupwise_quotient")
  x$add("fairness.npv", MeasureFairness, base_measure = msr("classif.npv"), operation = "groupwise_quotient")
  x$add("fairness.acc", MeasureFairness, base_measure = msr("classif.acc"), operation = "groupwise_quotient")
  x$add("fairness.fp", MeasureFairness, base_measure = msr("classif.fp"), operation = "groupwise_quotient")
  x$add("fairness.fn", MeasureFairness, base_measure = msr("classif.fn"), operation = "groupwise_quotient")
}
