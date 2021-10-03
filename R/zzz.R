#' @import mlr3
#' @import mlr3misc
#' @import R6
#' @import checkmate
#' @import mlr3measures
#' @import ggplot2
#' @import data.table
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
  x$task_col_roles = map(x$task_col_roles, function(cr) {
    union(cr, "pta")
  })

  x = getFromNamespace("mlr_measures", ns = "mlr3")
  x$add("fairness", MeasureFairness)

  # Define a set of widely used metrics. Documented in mlr_measures_fairness
  x$add("fairness.fpr", MeasureFairness, base_measure = msr("classif.fpr"))
  x$add("fairness.fnr", MeasureFairness, base_measure = msr("classif.fnr"))
  x$add("fairness.tpr", MeasureFairness, base_measure = msr("classif.tpr"))
  x$add("fairness.ppv", MeasureFairness, base_measure = msr("classif.ppv"))
  x$add("fairness.npv", MeasureFairness, base_measure = msr("classif.npv"))
  x$add("fairness.acc", MeasureFairness, base_measure = msr("classif.acc"))
  x$add("fairness.fp",  MeasureFairness, base_measure = msr("classif.fp"))
  x$add("fairness.fn",  MeasureFairness, base_measure = msr("classif.fn"))
  x$add("fairness.EOd", MeasureFairnessComposite, measures = list("fairness.fpr", "fairness.tpr"))
}
