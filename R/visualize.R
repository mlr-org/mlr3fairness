#' Fairness Accuracy Tradeoff Visualization
#'
#' @description
#' This function specialize in comparing the Fairness vs Accuracy between learners through visualizations.
#' From the visualization, users could see the tradeoff between fairness metrics and accuracy.
#' Those insights could help the users to choose the optimum model from their model sets.
#' And the fairness consistency of the learners by visualizing the tradeoff for different resampling iterations.
#' It could take multiple type of inputs, like (`Prediction()`) (`BenchmarkResult()`) and (`ResampleResult()`).
#'
#'
#' @param predcitions (`Prediction()`) (`BenchmarkResult()`) (`ResampleResult()`)\cr
#' The binary class prediction object that will be evaluated.
#' * If provided as (`Prediction()`). Then only one point will indicate the accuracy and fairness metrics for the current predictions.
#' * If provided as (`ResampleResult()`). Then the plot will compare the accuracy and fairness metrics for the same model, but different rasampling iterations.
#' * If provided as (`BenchmarkResult()`). Then the plot will compare the accuracy and fairness metrics for all models and all rasampling iterations.
#'
#' @param fairness_measure (`Measure()`)\cr
#' The fairness measures that will evaluated on predictions.
#'
#' @param data_task (`TaskClassif()`)\cr
#' The data task that contains the protected column, only required when the class of predcitions is (`Prediction()`)
#'
#' @export
#'
#' @examples
#'
#' # Setup the Fairness Measures and tasks
#' data_task = tsk("adult_train")
#' learner = lrn("classif.ranger", predict_type = "prob")
#' learner$train(data_task)
#' predictions = learner$predict(data_task)
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
#' fairness_accuracy_tradeoff(predictions, fairness_measure, data_task)
#' fairness_accuracy_tradeoff(bmr, fairness_measure)
#'
fairness_accuracy_tradeoff <- function(predcitions, ...){
  UseMethod("fairness_accuracy_tradeoff")
}

fairness_accuracy_tradeoff.Prediction <- function(predictions, fairness_measure, data_task){
  data = data.table(accuracy = predictions$score(msr("classif.acc")),
                    fairness = predictions$score(fairness_measure, data_task))
  ggplot(data, aes(x = accuracy, y=fairness)) + geom_point()
}

fairness_accuracy_tradeoff.BenchmarkResult <- function(predictions, fairness_measure){
  data = data.table(model = predictions$score(fairness_measure)[,6],
                    accuracy = predictions$score(msr("classif.acc"))[,11],
                    metrics = predictions$score(fairness_measure)[,11])
  colnames(data) <- c("model", "accuracy", "metrics")
  ggplot(data, aes(x = accuracy, y=metrics, colour=model)) +
    geom_point() +
    scale_color_aaas()
}

fairness_accuracy_tradeoff.ResampleResult <- function(predictions, fairness_measure){
  predictions = as_benchmark_result(predictions)
  fairness_accuracy_tradeoff(predcitions, fairness_measure)
}


#' Fairness Comparison
#'
#' @param predcitions
#' TODO: fill it
#'
#' @export
#'
#' @examples
#' NULL
fairness_compare <- function(predcitions, ...){
  UseMethod("fairness_compare")
}

fairness_compare.Prediction <- function(predcitions, fairness_measure, data_task){
  measures = predictions$score(fairness_measure, data_task)
  data <- melt(data.table(names = names(measures), data = measures), id.vars = "names")
  ggplot(data, aes(x=names, y=value, fill = names)) +
    geom_bar(stat = "identity",  width=0.4/length(unique(data$names))) +
    xlab("metrics") +
    ylab("values") +
    theme(legend.position = "none") +
    scale_fill_hue(c=100, l=60)
}

fairness_compare.BenchmarkResult <- function(predictions, fairness_measure){
  fairness_data = predictions$score(fairness_measure)
  data = melt(data.table(model = fairness_data$learner_id,
                         fairness_data[,c(11:ncol(fairness_data)), with=FALSE]),
              id.vars = "model")
  ggplot(data, aes(x=model, y=value, fill=model)) +
    geom_bar(stat = "identity",  width=2/length(unique(model))) +
    xlab("metrics") +
    ylab("values") +
    theme(legend.position = "none") +
    scale_fill_hue(c=100, l=60) +
    scale_color_aaas() +
    facet_wrap(~variable)
}

fairness_compare.ResampleResult <- function(predictions, fairness_measure){
  predictions = as_benchmark_result(predictions)
  fairness_compare(predictions, fairness_measure)
}

#' Fairness Prediction Density Visualization
#'
#' @description
#' This function specializes in visualizing the plots that will demonstrate the predicted probability density for each level in the protected column.
#' Which will potentially reveal the fairness problems in the dataset if the density differs significantly for each level.
#' The plot is a combination of boxplot and violin plot. So users could use the plot to detect both the outliers and distribution problems.
#' The y-axis shows the levels in protected columns. And the x-axis shows the predicted probability.
#' The title for the plot will demonstrate which class for predicted probability.
#'
#' @param predictions (`PredictionClassif()`)\cr
#' The binary class prediction object that will be evaluated
#'
#' @param data_task (`TaskClassif()`)\cr
#' The data task that contains the protected column.
#'
#' @export
#'
#' @examples
#' data_task = tsk("adult_train")
#' learner = lrn("classif.ranger", predict_type = "prob")
#' learner$train(data_task)
#' predictions = learner$predict(data_task)
#' fairness_prediction_density(predictions, data_task)
fairness_prediction_density <- function(predictions, data_task){
  data <- melt(data.table(data_task$data(cols = data_task$col_roles$pta),
                          predictions$prob[,1]),
               id = data_task$col_roles$pta)
  colnames(data) <- c("protected_variable", "variable", "probability")

  ggplot(data, aes(x = protected_variable, y=probability)) +
    geom_boxplot(aes(fill = protected_variable), width=0.4/length(unique(data$protected_variable))) +
    geom_violin(alpha = 0.3, width=1.5/length(unique(data$protected_variable)), fill = "grey") +
    xlab("protected variable") +
    ylab( paste0("predicted probability for ", colnames(predictions$prob)[1])) +
    theme(legend.position = "none") +
    scale_fill_hue(c=100, l=100) +
    ylim(c(0,1)) +
    coord_flip()
}
