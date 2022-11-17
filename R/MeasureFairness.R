#' @title Base Measure for Fairness
#'
#' @description
#' This measure extends [mlr3::Measure()] with statistical group fairness:
#' A common approach to quantifying a model's fairness is to compute the difference between a
#' protected and an unprotected group according w.r.t. some performance metric, e.g.
#' `classification error` ([mlr_measures_classif.ce]) or `false positive rate`
#' ([mlr_measures_classif.fpr]).
#' The operation for comparison (e.g., difference or quotient) can be specified using the `operation`
#' parameter, e.g. [groupdiff_absdiff()] or [groupdiff_tau()].
#'
#' Composite measures encompasing multiple fairness metrics can be built using
#' [MeasureFairnessComposite].
#'
#' Some popular predefined measures can be found in the [dictionary][mlr3misc::Dictionary] [mlr_measures].
#'
#' @template pta
#' 
#' @seealso [MeasureFairnessComposite]
#' @export
#' @examples
#' library("mlr3")
#' # Create MeasureFairness to measure the Predictive Parity.
#' t = tsk("adult_train")
#' learner = lrn("classif.rpart", cp = .01)
#' learner$train(t)
#' measure = msr("fairness", base_measure = msr("classif.ppv"))
#' predictions = learner$predict(t)
#' predictions$score(measure, task = t)
MeasureFairness = R6::R6Class("MeasureFairness", inherit = Measure,
  public = list(
    #' @template field_base_measure
    base_measure = NULL,

    #' @template field_operation
    operation = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param id (`character`)\cr
    #'   The measure's id. Set to 'fairness.<base_measure_id>' if ommited.
    #' @template param_base_measure
    #' @param operation (`function`)\cr
    #'   The operation used to compute the difference. A function that returns
    #'   a single value given input: computed metric for each subgroup.
    #'   Defaults to [groupdiff_absdiff].
    #' @param minimize (`logical()`)\cr
    #'   Should the measure be minimized? Defaults to `TRUE`.
    #' @param range (`numeric(2)`)\cr
    #'   Range of the resulting measure. Defaults to `c(-Inf, Inf)`.
    initialize = function(id = NULL, base_measure, operation = groupdiff_absdiff, minimize = TRUE,
      range = c(-Inf, Inf)) {
      self$operation = assert_function(operation)
      self$base_measure = assert_measure(as_measure(base_measure))

      if (is.null(id)) {
        id = replace_prefix(base_measure$id, mlr_reflections$task_types$type, "fairness.")
      }
      super$initialize(
        id = id,
        range = range,
        task_type = self$base_measure$task_type,
        properties = "requires_task",
        minimize = minimize,
        predict_type = base_measure$predict_type,
        packages = "mlr3fairness",
        man = "mlr_measures_fairness"
      )
    }
  ),

  private = list(
    .score = function(prediction, task, ...) {
      assert_pta_task(task)
      mvals = score_groupwise(prediction, self$base_measure, task, ...)
      invoke(self$operation, mvals)
    }
  )
)

mlr_measures$add("fairness", MeasureFairness)
