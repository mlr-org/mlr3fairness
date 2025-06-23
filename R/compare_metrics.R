#' Compare different metrics
#' 
#' @rdname fairness_compare_metrics
#'
#' @description
#' Compare learners with respect to to one or multiple metrics.
#' Metrics can but be but are not limited to fairness metrics.
#' 
#' @template pta
#'
#' @param object ([mlr3::PredictionClassif] | [mlr3::BenchmarkResult] | [mlr3::ResampleResult])\cr
#'   The object to create a plot for.
#'   * If provided a ([mlr3::PredictionClassif]).
#'     Then the visualization will compare the fairness metrics among the binary level from protected field
#'     through bar plots.
#'   * If provided a ([mlr3::ResampleResult]).
#'     Then the visualization will generate the boxplots for fairness metrics, and compare them among
#'     the binary level from protected field.
#'   * If provided a ([mlr3::BenchmarkResult]).
#'     Then the visualization will generate the boxplots for fairness metrics, and compare them among
#'     both the binary level from protected field and the models implemented.
#' @param ...
#' The arguments to be passed to methods, such as:
#'   * `fairness_measures` (list of [mlr3::Measure])\cr
#'     The fairness measures that will evaluated on object, could be single [mlr3::Measure] or list of [mlr3::Measure]s.
#'     Default measure set to be `msr("fairness.acc")`.
#'   * `task` ([mlr3::TaskClassif])\cr
#'     The data task that contains the protected column, only required when object is ([mlr3::PredictionClassif]).
#'
#' @export
#' @return A 'ggplot2' object.
#' @examplesIf rlang::is_installed("rpart") && rlang::is_installed("ranger")
#' library("mlr3")
#' library("mlr3learners")
#'
#' # Setup the Fairness Measures and tasks
#' task = tsk("adult_train")$filter(1:500)
#' learner = lrn("classif.ranger", predict_type = "prob")
#' learner$train(task)
#' predictions = learner$predict(task)
#' design = benchmark_grid(
#'   tasks = task,
#'   learners = lrns(c("classif.ranger", "classif.rpart"),
#'     predict_type = "prob", predict_sets = c("train", "test")),
#'   resamplings = rsmps("cv", folds = 3)
#' )
#'
#' bmr = benchmark(design)
#' fairness_measure = msr("fairness.tpr")
#' fairness_measures = msrs(c("fairness.tpr", "fairness.fnr", "fairness.acc"))
#'
#' # Predictions
#' compare_metrics(predictions, fairness_measure, task)
#' compare_metrics(predictions, fairness_measures, task)
#'
#' # BenchmarkResult and ResamplingResult
#' compare_metrics(bmr, fairness_measure)
#' compare_metrics(bmr, fairness_measures)
compare_metrics = function(object, ...) {
  UseMethod("compare_metrics")
}

#' @export
compare_metrics.PredictionClassif = function(object, measures = msr("fairness.acc"), task, ...) { # nolint
  measures = as_measures(measures)
  scores = setDT(as.data.frame(t(object$score(measures, task = task, ...))))
  data = melt(scores[, ids(measures), with = FALSE], measure.vars = names(scores))
  ggplot(data, aes(x = variable, y = value)) +
    geom_bar(stat = "identity") +
    xlab("Metrics") +
    ylab("Value") +
    theme(legend.position = "none") +
    scale_fill_hue(c = 100, l = 60)
}

#' @export
compare_metrics.BenchmarkResult = function(object, measures = msr("fairness.acc"), ...) { # nolint
  measures = as_measures(measures)
  scores = object$aggregate(measures, ...)
  data = melt(scores[, c(ids(measures), "learner_id", "task_id"), with = FALSE], id.vars = c("learner_id", "task_id"))
  ggplot(data, aes(x = learner_id, y = value, fill = variable)) +
    geom_bar(stat = "identity", position = "dodge") +
    xlab("Metrics") +
    ylab("Value") +
    scale_fill_hue(name = "Metric", c = 100, l = 60) +
    facet_wrap(~task_id)
}

#' @export
compare_metrics.ResampleResult = function(object, measures = msr("fairness.acc"), ...) { # nolint
  object = as_benchmark_result(object)
  compare_metrics(object, measures)
}


