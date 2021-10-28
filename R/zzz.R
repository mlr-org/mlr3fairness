#' @import mlr3
#' @import mlr3misc
#' @import R6
#' @import checkmate
#' @import mlr3pipelines
#' @import mlr3measures
#' @import ggplot2
#' @import paradox
#' @import data.table
#' @importFrom utils getFromNamespace data
#' @importFrom stats runif
"_PACKAGE"

.onLoad = function(libname, pkgname) { # nolint
  # register tasks
  x = getFromNamespace("mlr_tasks", ns = "mlr3") # nocov start
  x$add("adult_train", get_adult_task_train)
  x$add("adult_test", get_adult_task_test)
  x$add("compas", get_compas_task)

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
  x$add("fairness.composite", MeasureFairnessComposite)
  x$add("fairness.eod", MeasureFairnessComposite, measures = msrs(c("fairness.fpr", "fairness.tpr")),
    id = "equalized_odds")
  x$add("classif.pp", MeasurePositiveProbability)
  for (key in c("fn", "fnr", "fp", "fpr", "npv", "ppv", "tn", "tnr", "tp", "tpr")) {
    x$add(sprintf("fairness.%s", key), MeasureFairness,
      base_measure = msr(sprintf("classif.%s", key)))
  }

  x = getFromNamespace("mlr_pipeops", ns = "mlr3pipelines")
  x$add("reweighing_wts", PipeOpReweighingWeights)
  x$add("reweighing_os", PipeOpReweighingOversampling)
  x$add("EOd", PipeOpEOd)

  # static code checks should not complain about commonly used data.table columns
  utils::globalVariables(c("variable", "value", "learner_id", "n_tgt", "n_pta", "pta", "task_id",
    "pta_cols", "wt", "N", "agg")) # nocov end
}

mlr3misc::leanify_package()
