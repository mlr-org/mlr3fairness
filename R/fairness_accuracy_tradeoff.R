#' Plot Fairness Accuracy Trade-offs
#'
#' @description
#' Provides visualization wrt. trade-offs between fairness and accuracy metrics across learners and
#' resampling iterations.
#' This can assist in gauging the optimal model from a set of options along with estimates of variance
#' (through individual resampling iterations).
#'
#'
#' @param object ([PredictionClassif] | [BenchmarkResult] | [ResampleResult])\cr
#'   The binary class prediction object that will be evaluated.
#'   * If provided a [PredictionClassif].
#'     Then only one point will indicate the accuracy and fairness metrics for the current predictions.
#'     Requires also passing a [Task].
#'   * If provided a [ResampleResult].
#'     Then the plot will compare the accuracy and fairness metrics for the same model,
#'     but different resampling iterations  as well as the aggregate indicated by a cross.
#'   * If provided a [BenchmarkResult].
#'     Then the plot will compare the accuracy and fairness metrics for all models and all resampling iterations.
#'     Points are colored according to the learner_id and faceted by task_id.
#'     The aggregated score is indicated by a cross.
#'
#' @param ...
#'   Arguments to be passed to methods. Such as:
#'   * `fairness_measure` ([Measure])\cr
#'     The fairness measures that will evaluated.
#'     Default measure set to be `msr("fairness.fpr")`
#'   * `accuracy_measure` ([Measure])\cr
#'     The accuracy measure that will evaluated.
#'     Default measure set to be [msr("classif.acc")][mlr3::MeasureClassif].
#'   * `task` ([TaskClassif])\cr
#'     The data task that contains the protected column, only required when the class of object is ([PredictionClassif])
#'
#' @export
#' @return A 'ggplot2' object.
#' @examples
#' library(mlr3learners)
#' library(ggplot2)
#'
#' # Setup the Fairness measure and tasks
#' task = tsk("adult_train")$filter(1:500)
#' learner = lrn("classif.ranger", predict_type = "prob")
#' fairness_measure = msr("fairness.tpr")
#' 
#' # Example 1 - A single prediction
#' learner$train(task)
#' predictions = learner$predict(task)
#' fairness_accuracy_tradeoff(predictions, fairness_measure, task = task)
#' 
#' # Example2 - A benchmark
#' design = benchmark_grid(
#'   tasks = task,
#'   learners = lrns(c("classif.featureless", "classif.rpart"),
#'     predict_type = "prob", predict_sets = c("train", "test")),
#'   resamplings = rsmps("cv", folds = 2)
#' )
#' bmr = benchmark(design)
#' fairness_accuracy_tradeoff(bmr, fairness_measure)
fairness_accuracy_tradeoff = function(object, ...) {
  UseMethod("fairness_accuracy_tradeoff")
}

#' @export
fairness_accuracy_tradeoff.PredictionClassif = function(object, fairness_measure = msr("fairness.fpr"), acc_measure = msr("classif.acc"), task, ...) { # nolint
  assert_measure(fairness_measure)
  assert_measure(acc_measure)
  data = as.data.frame(t(object$score(list(acc_measure, fairness_measure), task = task)))
  ggplot(data, aes_string(x = acc_measure$id, y = fairness_measure$id)) +
    geom_point()
}

#' @export
fairness_accuracy_tradeoff.BenchmarkResult = function(object, fairness_measure = msr("fairness.fpr"), acc_measure = msr("classif.acc"), plot_scores = TRUE, ...) { # nolint
  assert_measure(fairness_measure)
  assert_measure(acc_measure)
  assert_flag(plot_scores)
  data = object$aggregate(list(acc_measure, fairness_measure))
  data = insert_named(data, list(aggi = 1, agg = "mean"))

  if (plot_scores) {
    tmp = object$score(list(acc_measure, fairness_measure))[, "aggi" := 0][, agg := "replication"]
    data = rbind(data, insert_named(tmp, list("aggi" = 0, agg = "replication")), fill = TRUE)
  }

  ggplot(data,
    aes_string(x = acc_measure$id, y = fairness_measure$id, colour = "learner_id", size = "aggi",
      alpha = "aggi", pch = "agg")) +
    geom_point() +
    scale_alpha(range = c(0.5, 1)) +
    scale_size(range = c(3, 6)) +
    scale_shape_manual(name = "Aggregation", values = c(4, 16)) +
    guides(size = "none", alpha = "none") +
    facet_wrap(~task_id)
}

#' @export
fairness_accuracy_tradeoff.ResampleResult = function(object, fairness_measure = msr("fairness.fpr"), acc_measure = msr("classif.acc"), ...) { # nolint
  object = as_benchmark_result(object)
  fairness_accuracy_tradeoff(object, fairness_measure, acc_measure)
}
