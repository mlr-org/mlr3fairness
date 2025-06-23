#' Probability Density Plot
#' @rdname fairness_prediction_density
#'
#' @description
#' Visualizes per-subgroup densities across learners, task and class.
#' The plot is a combination of boxplot and violin plot.
#' The y-axis shows the levels in protected columns. And the x-axis shows the predicted probability.
#' The title for the plot will demonstrate which class for predicted probability.
#' 
#' @template pta
#'
#' @param object ([mlr3::PredictionClassif] | [mlr3::ResampleResult] | [mlr3::BenchmarkResult])\cr
#'   The binary class prediction object that will be evaluated.
#'   If [mlr3::PredictionClassif], a [mlr3::Task] is required.
#' @param ...
#'   The arguments to be passed to methods, such as:
#'   * `task` ([mlr3::TaskClassif])\cr
#'     The data task that contains the protected column.
#'  * `type` [`character`]\cr
#'     The plot type. Either `violin` or `density`.
#'
#' @export
#' @return A 'ggplot2' object.
#' @examplesIf rlang::is_installed("rpart")
#' library("mlr3")
#' library("mlr3learners")
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
fairness_prediction_density.PredictionClassif = function(object, task,  type = "density", ...) { # nolint
  assert_choice(type, c("violin", "density"))
  if (is.null(object$prob)) {
    stop("Object needs to have predict.type = 'prob'!") # nocov
  }

  dt = as.data.table(object)
  dt = cbind(dt, task$data(rows = dt$row_ids, cols = task$col_roles$pta))
  dt[, task_id := task$id][, pta := task$col_roles$pta]
  classes = colnames(dt)[grep(colnames(dt), pattern = "^prob\\.")]
  dt = melt(dt, measure.vars = classes)
  dt = melt(dt, measure.vars = unique(dt$pta), value.name = "pta_cols", variable.name = "pta_name")

  # For binary get rid of 2nd class probs
  if (length(unique(classes)) == 2L) {
    dt = dt[variable == classes[1], ]
  }

  if (type == "violin") {
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
  } else if (type == "density") {
    ggplot(dt, aes(x = value)) +
      geom_density(aes(fill = pta_cols), alpha = 0.7) +
      xlab("Predicted probability") +
      labs(fill = "Protected Class") + 
      facet_wrap(variable ~ .)
  }
}

#' @export
fairness_prediction_density.BenchmarkResult = function(object, task, type = "density", ...) { # nolint
  if (object$task_type != "classif") {
    stopf("fairness_prediction_density() only works on classification problems")
  }

  predict_types = map_chr(object$learners$learner, "predict_type")
  if (any(predict_types != "prob")) {
    stopf("fairness_prediction_density() requires predict type 'prob'")
  }

  dt = rbindlist(map(object$resample_results$resample_result, function(rr) {
    dt = rbindlist(map(rr$predictions(), as.data.table))
    dt = cbind(setkey(dt), rr$task$data(cols = rr$task$col_roles$pta))
    dt[, task_id := rr$task$id][, learner_id := rr$learner$id][, pta := rr$task$col_roles$pta]
  }), fill = TRUE)

  # Melt probs
  classes = colnames(dt)[grep(colnames(dt), pattern = "prob.")]
  dt = melt(dt, measure.vars = classes)
  dt = melt(dt, measure.vars = unique(dt$pta), value.name = "pta_cols", variable.name = "pta_name")
  dt = dt[!is.na(value), ]

  # For binary get rid of class probs
  drop_cols = na.omit(dt[, .(binary = ifelse(length(unique(variable)) == 2L, unique(as.character(variable))[2], NA)), by = "task_id"][["binary"]])
  dt = dt[!(variable %in% drop_cols), ]
  # Combine names for better facetting  
  dt[, variable := paste0(task_id, ": ", variable)]
  
  if (type == "violin") {
    ggplot(dt, aes(x = pta_cols, y = value)) +
      geom_boxplot(aes(fill = pta_cols), width = 0.4 / length(unique(dt$pta_cols))) +
      geom_violin(alpha = 0.3, fill = "grey", width = 1.5 / length(unique(dt$pta_cols))) +
      xlab("Protected attributes") +
      scale_fill_hue(c = 100, l = 100) +
      ylim(c(0, 1)) +
      coord_flip() +
      facet_wrap(variable ~ learner_id)
  } else if (type == "density") {
    ggplot(dt, aes(x = value)) +
      geom_density(aes(fill = pta_cols), alpha = 0.7) +
      xlab("Predicted probability") +
      labs(fill = "Protected Class") + 
      facet_grid(variable ~ learner_id)
  }
}

#' @export
fairness_prediction_density.ResampleResult = function(object, task, type = "density", ...) { # nolint
  object = as_benchmark_result(object)
  fairness_prediction_density(object, task)
}

