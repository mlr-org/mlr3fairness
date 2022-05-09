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
#' @importFrom stats runif dist predict setNames na.omit
#' @keywords internal
"_PACKAGE"

.onLoad = function(libname, pkgname) { # nolint
  # register tasks
  x = getFromNamespace("mlr_tasks", ns = "mlr3") # nocov start
  x$add("adult_train", get_adult_task_train)
  x$add("adult_test", get_adult_task_test)
  x$add("compas", get_compas_task)
  x$add("compas_race_binary", get_compas_task_race_binary)

  # teach mlr3 about the new column role "pta" (protected attribute)
  x = getFromNamespace("mlr_reflections", ns = "mlr3")
  x$task_col_roles = map(x$task_col_roles, function(cr) {union(cr, "pta")})

  # register pipeop tag fairness
  x = utils::getFromNamespace("mlr_reflections", ns = "mlr3")
  x$pipeops$valid_tags = union(x$pipeops$valid_tags, "fairness")

  # Define a set of widely used metrics. Documented in mlr_measures_fairness
  x = getFromNamespace("mlr_measures", ns = "mlr3")
  # constructors
  x$add("fairness", MeasureFairness)
  x$add("fairness.composite", MeasureFairnessComposite)
  x$add("fairness.constraint", MeasureFairnessConstraint)
  x$add("classif.pp", MeasurePositiveProbability)
  x$add("subgroup", MeasureSubgroup)
  # regression
  for (key in c("mse")) {
    x$add(sprintf("fairness.%s", key), MeasureFairness,
      base_measure = msr(sprintf("regr.%s", key)), range = c(-Inf, Inf))
  }
  # rates classif
  for (key in c("acc", "fnr", "fpr", "tnr", "tpr", "npv", "ppv", "fomr")) {
    x$add(sprintf("fairness.%s", key), MeasureFairness,
      base_measure = msr(sprintf("classif.%s", key)), range = c(0, 1))
  }
  # counts
  for (key in c("fn", "fp", "tn", "tp")) {
    x$add(sprintf("fairness.%s", key), MeasureFairness,
      base_measure = msr(sprintf("classif.%s", key)))
  }

  x$add("fairness.cv", MeasureFairness, base_measure = msr("classif.pp"), range = c(0, 1), operation = groupdiff_diff)
  # compositions
  x$add("fairness.eod", MeasureFairnessComposite, measures = msrs(c("fairness.fpr", "fairness.tpr")), range = c(0, 1),
    id = "equalized_odds")
  x$add("fairness.pp", MeasureFairnessComposite, measures = msrs(c("fairness.ppv", "fairness.npv")), range = c(0, 1),
    id = "predictive_parity")
  x$add("fairness.acc_eod=.05", MeasureFairnessConstraint, performance_measure = msr("classif.acc"),
    fairness_measure = msr("fairness.eod"), epsilon = 0.05, id = "fairness.acc_eod=.05", range = c(-1, 1))
  x$add("fairness.acc_ppv=.05", MeasureFairnessConstraint, performance_measure = msr("classif.acc"),
    fairness_measure = msr("fairness.ppv"), epsilon = 0.05, id = "fairness.acc_eod=.05", range = c(-1, 1))

  x = getFromNamespace("mlr_pipeops", ns = "mlr3pipelines")
  x$add("reweighing_wts", PipeOpReweighingWeights)
  x$add("reweighing_os", PipeOpReweighingOversampling)
  x$add("EOd", PipeOpEOd)
  x$add("explicit_pta", PipeOpExplicitPta)
  
  x = getFromNamespace("mlr_learners", ns = "mlr3")
  x$add("regr.fairzlm", LearnerRegrFairzlm)
  x$add("classif.fairzlrm", LearnerClassifFairzlrm)
  x$add("regr.fairfrrm", LearnerRegrFairfrrm)
  x$add("classif.fairfgrrm", LearnerClassifFairfgrrm)
  x$add("regr.fairnclm", LearnerRegrFairnclm)

  # static code checks should not complain about commonly used data.table columns
  utils::globalVariables(c("variable", "value", "learner_id", "n_tgt", "n_pta", "pta", "task_id",
    "pta_cols", "wt", "N", "agg", "row_ids", "id", ".")) # nocov end
}

mlr3misc::leanify_package()
