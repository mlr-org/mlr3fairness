#' Fairness Accuracy Trade-off
#'
#' @description
#' Provides visualization wrt. trade-offs between fairness and accuracy metrics across learners and
#' resampling iterations.
#' This can assist in gauging the optimal model from a set of options along with estimates of variance
#' (through individual resampling iterations).
#'
#'
#' @param object ([PredictionClassif]) | ([BenchmarkResult]) | ([ResampleResult])\cr
#'   The binary class prediction object that will be evaluated.
#'   * If provided a ([PredictionClassif]).
#'     Then only one point will indicate the accuracy and fairness metrics for the current predictions.
#'     Requires also passing a [`Task`].
#'   * If provided a ([ResampleResult]).
#'     Then the plot will compare the accuracy and fairness metrics for the same model,
#'     but different resampling iterations  as well as the aggregate indicated by a cross.
#'   * If provided a ([BenchmarkResult]).
#'     Then the plot will compare the accuracy and fairness metrics for all models and all resampling iterations.
#'     Points are coloured according to the learner_id and faceted by task_id.
#'     The aggregated score is indicated by a cross.
#'
#' @param ...
#'   Arguments to be passed to methods. Such as:
#'   * `fairness_measure` ([Measure])\cr
#'     The fairness measures that will evaluated.
#'     Default measure set to be `msr("fairness.fpr")`
#'   * `accuracy_measure` ([Measure])\cr
#'     The accuracy measure that will evaluated.
#'     Default measure set to be [msr("classif.acc")][mlr3:MeasureClassif].
#'   * `task` ([TaskClassif])\cr
#'     The data task that contains the protected column, only required when the class of object is ([PredictionClassif])
#'
#' @export
#' @examples
#' library(mlr3)
#' library(mlr3fairness)
#' library(mlr3learners)
#' library(ggplot2)
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
#'
#' fairness_accuracy_tradeoff(predictions, fairness_measure, task = task)
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


#' Compare different metrics
#'
#' @description
#' Compare learners with respect to to one or multiple metrics.
#' Metrics can but be but are not limited to fairness metrics.
#'
#' @param object ([PredictionClassif]) | ([BenchmarkResult]) | ([ResampleResult])\cr
#'   The object to create a plot for.
#'   * If provided a ([PredictionClassif]).
#'     Then the visualization will compare the fairness metrics among the binary level from protected field
#'     through bar plots.
#'   * If provided a ([ResampleResult]).
#'     Then the visualization will generate the boxplots for fairness metrics, and compare them among
#'     the binary level from protected field.
#'   * If provided a ([BenchmarkResult]).
#'     Then the visualization will generate the boxplots for fairness metrics, and compare them among
#'     both the binary level from protected field and the models implemented.
#' @param ...
#' The arguments to be passed to methods, such as:
#'   * `fairness_measures` (list of [`Measure`])\cr
#'     The fairness measures that will evaluated on object, could be single measure \link{msr} or measures \link{msrs}.
#'     Default measure set to be \code{msr("fairness.acc")}
#'   * `task` ([TaskClassif])\cr
#'     The data task that contains the protected column, only required when object is ([PredictionClassif]).
#'
#' @export
#' @examples
#' library(mlr3)
#' library(mlr3fairness)
#' library(mlr3learners)
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
  scores = data.table(as.data.frame(t(object$score(measures, task = task, ...))))
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

#' Probability Density Plot
#'
#' @description
#' Visualizes per-subgroup densities across learners, task and class.
#' The plot is a combination of boxplot and violin plot.
#' The y-axis shows the levels in protected columns. And the x-axis shows the predicted probability.
#' The title for the plot will demonstrate which class for predicted probability.
#'
#' @param object ([PredictionClassif]) | ([ResampleResult]) | ([BenchmarkResult])\cr
#'   The binary class prediction object that will be evaluated.
#'   If [`PredictionClassif`], a [`Task`] is required.
#' @param ...
#'   The arguments to be passed to methods, such as:
#'   * `task` ([TaskClassif])\cr
#'     The data task that contains the protected column.
#'
#' @export
#' @examples
#' library(mlr3)
#' library(mlr3fairness)
#' library(mlr3learners)
#'
#' task = tsk("adult_train")$filter(1:500)
#' learner = lrn("classif.rpart", predict_type = "prob", cp = 0.001)
#' learner$train(task)
#'
#' # For prediction
#' predictions = learner$predict(task)
#' fairness_prediction_density(predictions, task)
#'
#' # For resampling
#' rr = resample(task, learner, rsmp("cv"))
#' fairness_prediction_density(rr)
fairness_prediction_density = function(object, ...) {
  UseMethod("fairness_prediction_density")
}

#' @export
fairness_prediction_density.PredictionClassif = function(object, task, ...) { # nolint
  if (is.null(object$prob)) {
    stop("Object needs to have predict.type = 'prob'!") # nocov
  }

  dt = as.data.table(object)
  dt = cbind(dt, task$data(rows = dt$row_ids, cols = task$col_roles$pta))
  dt[, task_id := task$id][, pta := task$col_roles$pta]
  classes = colnames(dt)[grep(colnames(dt), pattern = "prob.")]
  dt = melt(dt, measure.vars = classes)
  dt = melt(dt, measure.vars = unique(dt$pta), value.name = "pta_cols", variable.name = "pta_name")

  # For binary get rid of 2nd class probs
  if (length(unique(classes)) == 2L) {
    dt = dt[variable == classes[1], ]
  }

  ggplot(dt, aes(x = pta_cols, y = value)) +
    geom_boxplot(aes(fill = pta_cols), width = 0.4 / length(unique(dt$pta_cols))) +
    geom_violin(alpha = 0.3, width = 1.5 / length(unique(dt$pta_cols)), fill = "grey") +
    xlab("Protected attributes") +
    ylab("Predicted probability") +
    theme(legend.position = "none") +
    scale_fill_hue(name = "Subgroup", c = 100, l = 100) +
    ylim(c(0, 1)) +
    coord_flip() +
    facet_wrap(variable ~ .)
}

#' @export
fairness_prediction_density.BenchmarkResult = function(object, ...) { # nolint
  assert_true(all("prob" %in% map_chr(object$learners$learner, "predict_type")))

  dt = rbindlist(map(object$resample_results$resample_result, function(rr) {
    dt = rbindlist(map(rr$predictions(), as.data.table))
    dt = cbind(setkey(dt), rr$task$data(cols = rr$task$col_roles$pta))
    dt[, task_id := rr$task$id][, learner_id := rr$learner$id][, pta := rr$task$col_roles$pta]
  }), fill = TRUE)

  # Melt probs
  classes = colnames(dt)[grep(colnames(dt), pattern = "prob.")]
  dt = melt(dt, measure.vars = classes)
  dt = melt(dt, measure.vars = unique(dt$pta), value.name = "pta_cols", variable.name = "pta_name")

  # For binary get rid of 2nd class probs
  if (length(unique(classes)) == 2L) {
    dt = dt[variable == classes[1], ]
  }

  ggplot(dt, aes(x = pta_cols, y = value)) +
    geom_boxplot(aes(fill = pta_cols), width = 0.4 / length(unique(dt$pta_cols))) +
    geom_violin(alpha = 0.3, fill = "grey", width = 1.5 / length(unique(dt$pta_cols))) +
    xlab("Protected attributes") +
    scale_fill_hue(c = 100, l = 100) +
    ylim(c(0, 1)) +
    coord_flip() +
    facet_wrap(variable ~ learner_id + task_id)
}

#' @export
fairness_prediction_density.ResampleResult = function(object, task, ...) { # nolint
  object = as_benchmark_result(object)
  fairness_prediction_density(object, task)
}
