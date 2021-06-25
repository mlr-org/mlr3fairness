#' @title Fairness Measure
#'
#' @importFrom mlr3 Measure
#'
#' @description
#' This measure specializes [Measure] for Fairness Measure problems:
#'
#' Predefined measures can be found in the [dictionary][mlr3misc::Dictionary] [mlr_measures].
#'
#' @export
MeasureFairness = R6Class("MeasureFairness", inherit = Measure, cloneable = FALSE,
  public = list(
    #' @template field_fun
    fun = NULL,

    #' @template field_base_measure
    base_measure = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #' @param base_measure The measure used to perform fairness operations.
    initialize = function(base_measure) {
      info = mlr3fairness::measures[["groupwise_abs_diff"]]
      super$initialize(
        id = paste0("fairness.", base_measure$id),
        range = c(info$lower, info$upper),
        minimize = info$minimize,
        predict_type = base_measure$predict_type,
        packages = "mlr3fairness",
        man = paste0("mlr_measures_fairness.", base_measure$id),
        properties = info$properties
      )
      self$fun = get(name, envir = asNamespace("mlr3fairness"), mode = "function")
      self$base_measure = base_measure
    }
  ),

  private = list(
    .score = function(prediction, task, ...) {
      assert_prediction(prediction)
      print(task$man)
      if ("requires_task" %in% self$properties && is.null(task)) {
        stopf("Measure '%s' requires a task", self$id)
      }

      invoke(self$fun, prediction = prediction, na_value = self$na_value, data_task = task,
           base_measure = self$base_measure)
    }
  )
)

mlr_measures$add("fairness.groupwise_abs_diff", MeasureFairness, name = "groupwise_abs_diff")
