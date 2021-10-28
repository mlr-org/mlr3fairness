#' @title Fairness Measures
#'
#' @description
#'   This measure specializes [mlr3::Measure()] to allow for measuring statistical group fairness:
#'   A common approach to quantifying a model's fairness is to compute the difference between a
#'   protected and an unprotected group according to some performance metric such as
#'   `classification error` ([mlr_measures_classif.ce]), `false positive rate`
#'   ([mlr_measures_classif.fpr]) or others.
#'   This measure allows for the composition of a fairness-metric from such a "base_measure" by comparing
#'   the group-wise scores. The operation for comparison (e.g., difference or quotient) can be
#'   specified using the `operation` parameter.
#'
#' @section Overview:
#'   Composite measures encompasing multiple fairness metrics can be built using
#'   [MeasureFairnessComposite].
#'
#'   Predefined measures can be found in the [dictionary][mlr3misc::Dictionary] [mlr_measures].
#'
#' @seealso groupdiff_tau groupdiff_absdiff
#' @export
#' @examples
#' # Create MeasureFairness to measure the Predictive Parity.
#' data_task = tsk("adult_train")
#' learner = lrn("classif.rpart", cp = .01)
#' learner$train(data_task)
#' measure = msr("fairness", base_measure = msr("classif.ppv"))
#' predictions = learner$predict(data_task)
#' predictions$score(measure, task = data_task)
MeasureFairness = R6::R6Class("MeasureFairness", inherit = Measure, cloneable = FALSE,
  public = list(
    #' @template field_base_measure
    base_measure = NULL,

    #' @template field_operation
    operation = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param base_measure (`Measure()`)\cr
    #'   The measure used to perform fairness operations.
    #' @param operation (`function`)\cr
    #'   The operation used to compute the difference. A function that returns
    #'   a single value given input: computed metric for each subgroup.
    #'   Defaults to [groupdiff_absdiff].
    #' @param minimize (`logical`)\cr
    #'   Should the measure be minimized? Defaults to `TRUE`.
    #' @param range (`numeric(2)`)\cr
    #'   Range of the resulting measure. Defaults to `c(-Inf, Inf)`.
    initialize = function(base_measure, operation = groupdiff_absdiff, minimize = TRUE,
      range = c(-Inf, Inf)) {
      self$operation = assert_function(operation)
      self$base_measure = assert_measure(as_measure(base_measure))
      id = replace_prefix(base_measure$id, mlr_reflections$task_types$type, "fairness.")

      super$initialize(
        id = id,
        range = range,
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
      mvals = score_groupwise(prediction, self$base_measure, task)
      invoke(self$operation, mvals)
    }
  )
)

mlr_measures$add("fairness", MeasureFairness)


#' @title Composite Fairness Measure
#'
#' @description
#' Compute a composite measure from multiple fairness metrics.
#' Aggregates using `aggfun` (defaulting to [mean()]).
#'
#' @export
#' @examples
#' # Create MeasureFairness to measure the Predictive Parity.
#' # Equalized Odds Metric
#' MeasureFairnessComposite$new(measures = msrs(c("fairness.fpr", "fairness.tpr")))
#'
#' # Other metrics e.g. based on negative rates
#' MeasureFairnessComposite$new(measures = msrs(c("fairness.fnr", "fairness.tnr")))
MeasureFairnessComposite = R6::R6Class("MeasureFairnessComposite", inherit = Measure,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param id (`character(1)`)\cr
    #'   Id of the measure. Defaults to the concatenation of ids in `measure`.
    #' @param measures list of [`MeasureFairness`]\cr
    #'   List of fairness measures to aggregate.
    #' @param aggfun (`function()`)\cr
    #'   Aggregation function used to aggregate results from respective measures. Defaults to `sum`.
    #' @param operation (`function()`)\cr
    #'   The operation used to compute the difference. A function that returns
    #'   a single value given input: computed metric for each subgroup.
    #'   Defaults to `groupdiff_absdiff`.
    #'   See `MeasureFairness` for more information.
    #' @param minimize (`logical(1)`)\cr
    #'   Should the measure be minimized? Defaults to `TRUE`.
    #' @param range (`numeric(2)`)\cr
    #'   Range of the resulting measure. Defaults to `c(-Inf, Inf)`.
    initialize = function(id = NULL, measures, aggfun = function(x) mean(x, na.rm = TRUE),
      operation = groupdiff_absdiff, minimize = TRUE, range = c(-Inf, Inf)) {

      private$.measures = assert_measures(as_measures(measures))
      private$.aggfun = assert_function(aggfun)

      if (is.null(id)) {
        id = paste0(replace_prefix(ids(measures), c(mlr_reflections$task_types$type, "fairness"), ""),
          collapse = "_")
      }

      super$initialize(
        id = sprintf("fairness.%s", assert_string(id)),
        range = assert_numeric(range, len = 2),
        properties = "requires_task",
        minimize = assert_flag(minimize),
        predict_type = unique(unlist(map(measures, "predict_type"))),
        packages = "mlr3fairness",
        man = "mlr_measures_fairness_composite"
      )
    }
  ),

  private = list(
    .measures = NULL,
    .aggfun = NULL,
    .score = function(prediction, task, ...) {
      private$.aggfun(
        # FIXME
        map_dbl(private$.measures, function(m) {
          prediction$score(m, task = task, ...)
        })
      )
    }
  )
)

#' @title Positive Probability Measure
#' @name mlr_measures_positive_probability
#'
#' @description
#' Return the positive probability of the predictions.
#' This is defined as count of positive predictions divided by the number of observations.
#'
#' @export
#' @examples
#' # Create Positive Probability Measure
#' data_task = tsk("adult_train")
#' learner = lrn("classif.rpart", cp = .01)
#' learner$train(data_task)
#' measure = msr("classif.pp")
#' predictions = learner$predict(data_task)
#' predictions$score(measure, task = data_task)
MeasurePositiveProbability = R6::R6Class("MeasurePositiveProbability",
  inherit = mlr3::Measure,
  public = list(

    #' @description
    #' Initialize a Measure Positive Probability Object
    initialize = function() {
      super$initialize(
        id = "classif.pp",
        predict_type = "response",
        range = c(0, 1),
        minimize = FALSE)
    }
  ),

  private = list(
    .score = function(prediction, task, ...) {
      mean(prediction$response == task$positive)
    }
  )
)

mlr_measures$add("classif.pp", MeasurePositiveProbability)
