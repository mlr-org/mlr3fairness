#' Fairness Accuracy Tradeoff Visualization
#'
#' @description
#' These functions specialize in comparing the Fairness and Accuracy between learners through visualizations.
#' From the visualization, users could see the tradeoff between fairness metrics and accuracy.
#' Those insights could help the users to choose the optimum model from their model sets.
#' And the fairness consistency of the learners by visualizing the tradeoff for different resampling iterations.
#' It could take multiple types of inputs, like ([PredictionClassif]), ([BenchmarkResult]) and ([ResampleResult]).
#'
#'
#' @param object ([PredictionClassif])\cr ([BenchmarkResult])\cr ([ResampleResult])\cr
#' The binary class prediction object that will be evaluated. Only one data task is allowed for BenchmarkResult or ResampleResult
#' * If provided a ([PredictionClassif]). Then only one point will indicate the accuracy and fairness metrics for the current predictions.
#' * If provided a ([ResampleResult]). Then the plot will compare the accuracy and fairness metrics for the same model, but different resampling iterations.
#' * If provided a ([BenchmarkResult]). Then the plot will compare the accuracy and fairness metrics for all models and all resampling iterations. Points are coloured acording to the models.
#'
#' @param ...
#' Arguments to be passed to methods. Such as:
#'
#' * `fairness_measure` ([Measure])\cr
#' The fairness measures that will evaluated on object, could be single measure (`msr`) or list of measures (`msrs`), check [mlr3::Measure]. Default measure set to be \code{msr("fairness.fpr")}
#'
#' * `accuracy_measure` ([Measure])\cr
#' The accuracy measure that will evaluated on object, could be single measure \link{msr} or measures \link{msrs}. Default measure set to be \code{\link[mlr3:MeasureClassif]{msr("classif.acc")}}
#'
#' * `task` ([TaskClassif])\cr
#' The data task that contains the protected column, only required when the class of object is ([PredictionClassif])
#'
#' @examples
#' library(mlr3fairness)
#' library(mlr3learners)
#' library(mlr3)
#' library(ggplot2)
#' library(data.table)
#'
#' # Setup the Fairness Measures and tasks
#' task = tsk("adult_train")
#' learner = lrn("classif.ranger", predict_type = "prob")
#' learner$train(task)
#' predictions = learner$predict(task)
#' design = benchmark_grid(
#'   tasks = tsk("adult_train"),
#'   learners = lrns(c("classif.ranger", "classif.rpart"),
#'                  predict_type = "prob", predict_sets = c("train", "test")),
#'   resamplings = rsmps("cv", folds = 3)
#' )
#'
#' bmr = benchmark(design)
#' fairness_measure = msr("fairness.tpr")
#'
#' fairness_accuracy_tradeoff(predictions, fairness_measure, task)
#' fairness_accuracy_tradeoff(bmr, fairness_measure)
#' @export
fairness_accuracy_tradeoff <- function(object, ...){
  UseMethod("fairness_accuracy_tradeoff")
}

#' @export
fairness_accuracy_tradeoff.PredictionClassif <- function(object, fairness_measure = msr("fairness.fpr"), task, acc_measure = msr("classif.acc"), ...){
  data = data.table(accuracy = object$score(acc_measure),
                    fairness = object$score(fairness_measure, task)) #Create a data table to store both fairness and acc metrics.
  ggplot(data, aes(x = accuracy, y=fairness)) +
    labs(y = fairness_measure$id, x=acc_measure$id) +
    geom_point()
}

#' @export
fairness_accuracy_tradeoff.BenchmarkResult <- function(object, fairness_measure = msr("fairness.fpr"), acc_measure = msr("classif.acc"), ...){
  data = data.table(model = object$score(fairness_measure)[, "learner_id"][[1]],
                    accuracy = object$score(acc_measure)[, acc_measure$id, with = F],
                    metrics = object$score(fairness_measure)[, fairness_measure$id, with = F])
  colnames(data) <- c("model", "accuracy", "metrics")
  ggplot(data, aes(x = accuracy, y=metrics, colour=model)) +
    labs(y = fairness_measure$id, x=acc_measure$id) +
    geom_point()
}

#' @export
fairness_accuracy_tradeoff.ResampleResult <- function(object, fairness_measure = msr("fairness.fpr"), acc_measure = msr("classif.acc"), ...){
  object = as_benchmark_result(object)
  fairness_accuracy_tradeoff(object, fairness_measure, acc_measure)
}


#' Fairness Comparison Visualization
#'
#' @description
#' These functions specialize in comparing the Fairness metrics between learners and levels in protected columns through visualizations.
#' From the visualization, users could see the fairness metrics difference between learners and levels in protected fields.
#' Those visualizations could help the users to detect the fairness problems and choose the optimum model.
#' It could take multiple type of inputs, like ([PredictionClassif]), ([BenchmarkResult]) and ([ResampleResult]).
#'
#' @param object ([PredictionClassif])\cr ([BenchmarkResult])\cr ([ResampleResult])\cr
#' The binary class prediction object that will be evaluated. Only one data task is allowed for BenchmarkResult or ResampleResult
#' * If provided a ([PredictionClassif]). Then the visualization will compare the fairness metrics among the binary level from protected field through bar plots.
#' * If provided a ([ResampleResult]). Then the visualization will generate the boxplots for fairness metrics, and compare them among the binary level from protected field.
#' * If provided a ([BenchmarkResult]). Then the visualization will generate the boxplots for fairness metrics, and compare them among both the binary level from protected field and the models implemented.
#'
#' @param ...
#' The arguments to be passed to methods, such as:
#'
#' * `fairness_measure` ([Measure])\cr
#' The fairness measures that will evaluated on object, could be single measure \link{msr} or measures \link{msrs}.  Default measure set to be \code{msr("fairness.acc")}
#'
#' * `task` ([TaskClassif])\cr
#' The data task that contains the protected column, only required when the class of object is ([PredictionClassif])
#'
#' @examples
#' library(mlr3fairness)
#' library(mlr3learners)
#' library(mlr3)
#' library(ggplot2)
#' library(data.table)
#'
#' # Setup the Fairness Measures and tasks
#' task = tsk("adult_train")
#' learner = lrn("classif.ranger", predict_type = "prob")
#' learner$train(task)
#' predictions = learner$predict(task)
#' design = benchmark_grid(
#'   tasks = tsk("adult_train"),
#'   learners = lrns(c("classif.ranger", "classif.rpart"),
#'                  predict_type = "prob", predict_sets = c("train", "test")),
#'   resamplings = rsmps("cv", folds = 3)
#' )
#'
#' bmr = benchmark(design)
#' fairness_measure = msr("fairness.tpr")
#' fairness_measures = msrs(c("fairness.tpr", "fairness.fnr"))
#'
#' fairness_compare(predictions, fairness_measure, task)
#' fairness_compare(predictions, fairness_measures, task)
#' fairness_compare(bmr, fairness_measure)
#' fairness_compare(bmr, fairness_measures)
#' @export
fairness_compare <- function(object, ...){
  UseMethod("fairness_compare")
}

#' @export
fairness_compare.PredictionClassif <- function(object, fairness_measure = msr("fairness.acc"), task, ...){
  measures = object$score(fairness_measure, task)
  data <- melt(data.table(names = names(measures), data = measures), id.vars = "names")
  ggplot(data, aes(x=names, y=value, fill = names)) +
    geom_bar(stat = "identity",  width=0.4/length(unique(data$names))) +
    xlab("metrics") +
    ylab("values") +
    theme(legend.position = "none") +
    scale_fill_hue(c=100, l=60)
}

#' @export
fairness_compare.BenchmarkResult <- function(object, fairness_measure = msr("fairness.acc"), ...){
  fairness_name = ids(fairness_measure)
  fairness_data = object$score(fairness_measure)
  data = melt(data.table(model = fairness_data$learner_id,
                         fairness_data[, fairness_name, with=FALSE]),
              id.vars = "model")
  ggplot(data, aes(x=model, y=value, fill=model)) +
    geom_bar(stat = "identity",  width=1/length(unique(data$model))) +
    xlab("metrics") +
    ylab("values") +
    theme(legend.position = "none") +
    scale_fill_hue(c=100, l=60) +
    facet_wrap(~variable)
}

#' @export
fairness_compare.ResampleResult <- function(object, fairness_measure = msr("fairness.acc"), ...){
  object = as_benchmark_result(object)
  fairness_compare(object, fairness_measure)
}

#' Fairness Prediction Density Visualization
#'
#' @description
#' These functions specialize in visualizing the plots that will demonstrate the predicted probability density for each level in the protected column.
#' Which will potentially reveal the fairness problems in the dataset if the density differs significantly for each level.
#' The plot is a combination of boxplot and violin plot. So users could use the plot to detect both the outliers and distribution problems.
#' The y-axis shows the levels in protected columns. And the x-axis shows the predicted probability.
#' The title for the plot will demonstrate which class for predicted probability.
#'
#' @param object ([PredictionClassif])\cr ([ResampleResult])\cr ([BenchmarkResult])\cr
#' The binary class prediction object that will be evaluated. Only one data task is allowed for Resample Result and Benchmark Result.
#'
#' @param ...
#' The arguments to be passed to methods, such as:
#'
#' * `task` ([TaskClassif])\cr
#' The data task that contains the protected column.
#'
#' @examples
#' library(mlr3fairness)
#' library(mlr3learners)
#' library(mlr3)
#' library(ggplot2)
#' library(data.table)
#'
#' task = tsk("adult_train")
#' learner = lrn("classif.ranger", predict_type = "prob")
#' learner$train(task)
#' predictions = learner$predict(task)
#' fairness_prediction_density(predictions, task)
#' @export
fairness_prediction_density <- function(object, ...){
  UseMethod("fairness_prediction_density")
}

#' @export
fairness_prediction_density.PredictionClassif<- function(object, task, ...){
  data <- melt(data.table(task$data(cols = task$col_roles$pta),
                          object$prob[,1]),
               id = task$col_roles$pta)
  colnames(data) <- c("protected_variable", "variable", "probability")

  ggplot(data, aes(x = protected_variable, y=probability)) +
    geom_boxplot(aes(fill = protected_variable), width=0.4/length(unique(data$protected_variable))) +
    geom_violin(alpha = 0.3, width=1.5/length(unique(data$protected_variable)), fill = "grey") +
    xlab("Protected attributes") +
    ylab( paste0("predicted probability for ", colnames(object$prob)[1])) +
    theme(legend.position = "none") +
    scale_fill_hue(c=100, l=100) +
    ylim(c(0,1)) +
    coord_flip()
}

#' @export
fairness_prediction_density.BenchmarkResult <- function(object, task, ...){
  NULL
}

#' @export
fairness_prediction_density.ResampleResult <- function(object, task, ...){
  object = as_benchmark_result(object)
  fairness_prediction_density(object, task)
}
