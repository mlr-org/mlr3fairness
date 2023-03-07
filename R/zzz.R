#' @import R6
#' @import data.table
#' @import checkmate
#' @import mlr3
#' @import mlr3misc
#' @import mlr3measures
#' @import mlr3pipelines
#' @import mlr3learners
#' @import ggplot2
#' @import paradox
#' @importFrom utils getFromNamespace data
#' @importFrom stats runif dist predict setNames na.omit
#' @importFrom rlang .data
#' @keywords internal
"_PACKAGE"

register_mlr3 = function() {
  # nocov start
  # teach mlr3 about the new column role "pta" (protected attribute)
  x = getFromNamespace("mlr_reflections", ns = "mlr3")
  x$task_col_roles = map(x$task_col_roles, function(cr) union(cr, "pta"))
  x$task_print_col_roles$after = c(x$task_print_col_roles$after, c("Protected attribute" = "pta"))

  # register tasks
  tasks[["adult_train"]] = get_adult_task_train()
  tasks[["adult_test"]] = get_adult_task_test()
  tasks[["compas"]] = get_compas_task()
  tasks[["compas_race_binary"]] = get_compas_task_race_binary()
  x = getFromNamespace("mlr_tasks", ns = "mlr3")
  iwalk(tasks, function(o, nm) {x$add(nm, o)})

  # register pipeop tag fairness
  x = utils::getFromNamespace("mlr_reflections", ns = "mlr3")
  x$pipeops$valid_tags = union(x$pipeops$valid_tags, "fairness")

  # Define a set of widely used metrics. Documented in mlr_measures_fairness
  x = getFromNamespace("mlr_measures", ns = "mlr3")
  # constructor
  measures[["fairness"]] = MeasureFairness
  measures[["fairness.composite"]] = MeasureFairnessComposite
  measures[["fairness.constraint"]] = MeasureFairnessConstraint
  measures[["classif.pp"]] = MeasurePositiveProbability
  measures[["subgroup"]] = MeasureSubgroup
  # regression
  for (key in c("mse")) {
    measures[[sprintf("fairness.%s", key)]] = MeasureFairness$new(base_measure = msr(sprintf("regr.%s", key)), range = c(-Inf, Inf))
  }
  # rates classif
  for (key in c("acc", "fnr", "fpr", "tnr", "tpr", "npv", "ppv", "fomr")) {
    measures[[sprintf("fairness.%s", key)]] = MeasureFairness$new(base_measure = msr(sprintf("classif.%s", key)), range = c(0, 1))
  }
  # counts
  for (key in c("fn", "fp", "tn", "tp")) {
    measures[[sprintf("fairness.%s", key)]] = MeasureFairness$new(base_measure = msr(sprintf("classif.%s", key)))
  }

  iwalk(measures, function(o, nm) {x$add(nm, o)})
  measures[["fairness.cv"]] = MeasureFairness$new(base_measure = msr("classif.pp"), range = c(0, 1), operation = groupdiff_absdiff)
  # compositions
  measures[["fairness.eod"]] = MeasureFairnessComposite$new(measures = msrs(c("fairness.fpr", "fairness.tpr")), range = c(0, 1),
    id = "equalized_odds")
  measures[["fairness.pp"]] = MeasureFairnessComposite$new(measures = msrs(c("fairness.ppv", "fairness.npv")), range = c(0, 1),
    id = "predictive_parity")
  iwalk(measures, function(o, nm) {x$add(nm, o)})

  measures[["fairness.acc_eod=.05"]] = MeasureFairnessConstraint$new(performance_measure = msr("classif.acc"),
    fairness_measure = msr("fairness.eod"), epsilon = 0.05, id = "fairness.acc_eod=.05", range = c(-1, 1))
  measures[["fairness.acc_ppv=.05"]] = MeasureFairnessConstraint$new(performance_measure = msr("classif.acc"),
    fairness_measure = msr("fairness.ppv"), epsilon = 0.05, id = "fairness.acc_ppv=.05", range = c(-1, 1))
  x = getFromNamespace("mlr_measures", ns = "mlr3")
  iwalk(measures, function(o, nm) {x$add(nm, o)})


  pipeops[["reweighing_wts"]] = PipeOpReweighingWeights
  pipeops[["reweighing_os"]] = PipeOpReweighingOversampling
  pipeops[["EOd"]] = PipeOpEOd
  pipeops[["explicit_pta"]] = PipeOpExplicitPta
  x = getFromNamespace("mlr_pipeops", ns = "mlr3pipelines")
  iwalk(pipeops, function(o, nm) {x$add(nm, o)})

  learners[["regr.fairzlm"]] = LearnerRegrFairzlm
  learners[["classif.fairzlrm"]] = LearnerClassifFairzlrm
  learners[["regr.fairfrrm"]] = LearnerRegrFairfrrm
  learners[["classif.fairfgrrm"]] = LearnerClassifFairfgrrm
  learners[["regr.fairnclm"]] = LearnerRegrFairnclm
  x = getFromNamespace("mlr_learners", ns = "mlr3")
  iwalk(learners, function(o, nm) {x$add(nm, o)})
} # nocov end

.onLoad = function(libname, pkgname) { # nolint
  # nocov start
  register_namespace_callback(pkgname, "mlr3", register_mlr3)
} # nocov end

.onUnload = function(libpath) { # nolint
  # nocov start
  # Delete objects
  mlr_tasks = mlr3::mlr_tasks
  walk(names(tasks), function(id) mlr_tasks$remove(id))
  mlr_measures = mlr3::mlr_measures
  walk(names(measures), function(id) mlr_measures$remove(id))
  mlr_learners = mlr3::mlr_learners
  walk(names(learners), function(id) mlr_learners$remove(id))
  mlr_pipeops = mlr3pipelines::mlr_pipeops
  walk(names(pipeops), function(id) mlr_pipeops$remove(id))
} # nocov end

# static code checks should not complain about commonly used data.table columns
# nocov start
utils::globalVariables(c("variable", "value", "learner_id", "n_tgt", "n_pta", "pta", "task_id", 
  "pta_cols", "wt", "N", "agg", "row_ids", "id", ".")) 
# nocov end
mlr3misc::leanify_package()
