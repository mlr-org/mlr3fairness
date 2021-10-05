#' @import mlr3
#' @import mlr3misc
#' @import R6
#' @import checkmate
#' @import mlr3measures
#' @import mlr3pipelines
#' @import ggplot2
#' @importFrom utils getFromNamespace data
#' @import paradox
#' @import data.table
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

  # register pipeop tag fairness
  x = utils::getFromNamespace("mlr_reflections", ns = "mlr3")
  x$pipeops$valid_tags = union(x$pipeops$valid_tags, "fairness")

  # Define a set of widely used metrics. Documented in mlr_measures_fairness
  x = getFromNamespace("mlr_measures", ns = "mlr3")
  x$add("fairness", MeasureFairness)
  x$add("fairness.fpr", MeasureFairness, base_measure = msr("classif.fpr"))
  x$add("fairness.fnr", MeasureFairness, base_measure = msr("classif.fnr"))
  x$add("fairness.tpr", MeasureFairness, base_measure = msr("classif.tpr"))
  x$add("fairness.tnr", MeasureFairness, base_measure = msr("classif.tnr"))
  x$add("fairness.ppv", MeasureFairness, base_measure = msr("classif.ppv"))
  x$add("fairness.npv", MeasureFairness, base_measure = msr("classif.npv"))
  x$add("fairness.acc", MeasureFairness, base_measure = msr("classif.acc"))
  x$add("fairness.fp",  MeasureFairness, base_measure = msr("classif.fp"))
  x$add("fairness.fn",  MeasureFairness, base_measure = msr("classif.fn"))
  x$add("fairness.eod", MeasureFairnessComposite, measures = list("fairness.fpr", "fairness.tpr"))
  x$add("classif.pp", MeasurePositiveProbability)

  x = getFromNamespace("mlr_pipeops", ns = "mlr3pipelines")
  x$add("reweighing_wts", PipeOpReweighingWeights)
  x$add("reweighing_os", PipeOpReweighingOversampling)

  # static code checks should not complain about commonly used data.table columns
  utils::globalVariables(c("variable", "value", "learner_id", "n_tgt", "n_pta", 'pta', 'task_id', 'pta_cols', 'wt', 'N', 'agg'))

}
