#' @title Fairness Measure
#' @name mlr_measures_fairness
#'
#' @description
#' This measure specializes [Measure] for Fairness Measure problems:
#' Users could use ["groupwise_abs_diff", "groupwise_diff", "groupwise_quotient"] to evaluate
#' the fairness measures based on base_measures. For example, the false positive rate bias or
#' the equalized odds ratios. More examples could viewed on mlr3fairness wikipage.
#'
#' Predefined measures can be found in the [dictionary][mlr3misc::Dictionary] [mlr_measures].
#' Predifined operations can be found in the [mlr3fairness].
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
    #' @param operation The operation name performed on the base measures.
    #' @param ps The parameter sets.
    initialize = function(operation, base_measure, ps = ps()) {
      super$initialize(
        id = paste0("fairness.", base_measure$id),
        range = c(-Inf, Inf),
        minimize = FALSE,
        predict_type = base_measure$predict_type,
        packages = "mlr3fairness",
        man = paste0("mlr_measures_fairness")
      )
      self$fun = get(operation, envir = asNamespace("mlr3fairness"), mode = "function")
      self$base_measure = base_measure
    }
  ),

  private = list(
    .score = function(prediction, task, ...) {
      assert_prediction(prediction)
      if ("requires_task" %in% self$properties && is.null(task)) {
        stopf("Measure '%s' requires a task", self$id)
      }

      invoke(self$fun, prediction = prediction, na_value = self$na_value, data_task = task,
           base_measure = self$base_measure)
    }
  )
)

mlr_measures$add("mlr_measures_fairness", MeasureFairness, name = "mlr_measures_fairness")
