#' @title Fairness Measure
#'
#' @description
#' This measure specializes [Measure] for fairness measures:
#'
#' * Possible values for `predict_type` are `"response"` and `"prob"`.
#'
#' Predefined measures can be found in the [dictionary][mlr3misc::Dictionary] [mlr_measures].
#' The default measure for classification is [`classif.ce`][mlr_measures_classif.ce].
#'
#' @template param_fun
#' @template param_base_measure
#' @template param_na_value
#'
#' @export
MeasureFairness = R6Class("MeasureFairness", inherit = Measure, cloneable = FALSE,
  public = list(
    fun = NULL,
    na_value = NaN,
    base_measure = NULL,
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(name, base_measure) {
      info = mlr3fairness::measures[["groupwise_abs_diff"]]
      super$initialize(
        id = paste0("fairness.", base_measure$id),
        range = c(info$lower, info$upper),
        minimize = info$minimize,
        predict_type = info$predict_type,
        packages = "mlr3fairness",
        man = paste0("mlr_measures_fairness.", name)
      )
      self$fun = get(name, envir = asNamespace("mlr3fairness"), mode = "function")
      self$base_measure = base_measure
    }
  ),

  private = list(
    .score = function(prediction, task, ...) {
      invoke(self$fun, prediction = prediction, na_value = self$na_value, data_task = task,
           base_measure = self$base_measure)
    }
  )
)

mlr_measures$add("fairness.groupwise_abs_diff", MeasureFairness, name = "groupwise_abs_diff")
