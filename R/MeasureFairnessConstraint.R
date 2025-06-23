#' @title Fairness Constraint Measure
#'
#' @description
#'   This measure allows constructing for 'constraint' measures of the following form:\cr
#'   \deqn{min performance subject to fairness < \epsilon}
#'
#' @template pta
#' @seealso mlr_measures_fairness
#' @export
#' @examplesIf rlang::is_installed("rpart")
#' # Accuracy subject to equalized odds fairness constraint:
#' library("mlr3")
#' t = tsk("adult_train")
#' learner = lrn("classif.rpart", cp = .01)
#' learner$train(t)
#' measure = msr("fairness.constraint", id = "acc_tpr", msr("classif.acc"), msr("fairness.tpr"))
#' predictions = learner$predict(t)
#' predictions$score(measure, task = t)
MeasureFairnessConstraint = R6::R6Class("MeasureFairnessConstraint", inherit = Measure,
  public = list(
    #' @field performance_measure (`Measure()`)\cr
    #' The performance measure to be used.
    performance_measure = NULL,
    #' @field fairness_measure (`Measure()`)\cr
    #' The fairness measure to be used.
    fairness_measure = NULL,
    #' @field epsilon (`numeric`)\cr
    #' Deviation from perfect fairness that is allowed.
    epsilon = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #' @param id (`character`)\cr
    #'   The measure's id. Set to 'fairness.<base_measure_id>' if ommited.
    #' @param performance_measure (`Measure()`)\cr
    #' The measure used to measure performance (e.g. accuracy).
    #' @param fairness_measure (`Measure()`)\cr
    #' The measure used to measure fairness (e.g. equalized odds).
    #' @param epsilon (`numeric`)\cr
    #' Allowed divergence from perfect fairness. Initialized to 0.01.
    #' @param range (`numeric`)\cr
    #' Range of the resulting measure. Defaults to `c(-Inf, Inf)`.
    initialize = function(
      id = NULL,  performance_measure, fairness_measure, epsilon = 0.01, range = c(-Inf, Inf)) {
      self$performance_measure = assert_measure(performance_measure)
      self$fairness_measure = assert_measure(fairness_measure)
      assert_true(all(self$performance_measure$task_type == self$fairness_measure$task_type))
      self$epsilon = assert_number(epsilon)

      # fix up prefixes: regr|classif|... to fairness
      metrics_short = gsub(
        paste0(c(mlr_reflections$task_types$type, "fairness"), collapse = "|"),
        "", c(performance_measure$id, fairness_measure$id))
      if (is.null(id)) {
        id = paste0("fairness.", paste0(gsub("\\.", "", metrics_short), collapse = "_"), "_cstrt")
      }
      super$initialize(
        id = id,
        range = assert_numeric(range, len = 2),
        properties = "requires_task",
        task_type = self$performance_measure$task_type,
        minimize = assert_flag(self$performance_measure$minimize),
        predict_type = performance_measure$predict_type,
        packages = "mlr3fairness",
        man = "mlr_measures_fairness_constraint"
      )
    }
  ),

  private = list(
    .score = function(prediction, task, ...) {
      assert_pta_task(task)
      eps = self$epsilon
      if (!self$fairness_measure$minimize) { # nocov start
        stop("Only minimized fairness measures are currently supported!")
      } # nocov end
      args = list(...)
      args$weights = NULL
      fair = invoke(self$fairness_measure$score, prediction, task, .args = args)
      perf = invoke(self$performance_measure$score, prediction, task, .args = args)

      assert_number(perf, lower = 0)
      prange = self$performance_measure$range
      frange = self$fairness_measure$range
      opt_fairness = ifelse(self$fairness_measure$minimize, min(frange), max(frange))

      if (is.infinite(opt_fairness)) warning("Fairness measure has infinite range!")
      is_fair = abs(opt_fairness - fair) < eps
      if (self$minimize) {
        out = (!is_fair) * (max(prange) + fair) + (is_fair) * perf
      } else {
        out = (!is_fair) * (min(prange) - fair) + (is_fair) * perf
      }
      return(out)
    }
  )
)
