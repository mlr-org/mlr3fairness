#' @title Composite Fairness Measure
#'
#' @description
#' Computes a composite measure from multiple fairness metrics and aggregates them
#' using `aggfun` (defaulting to [mean()]).
#'
#' @template pta
#' 
#' @export
#' @examples
#' library("mlr3")
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
    #' @param measures (list of [MeasureFairness])\cr
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
    initialize = function(id = NULL, measures, aggfun = function(x) mean(x),
      operation = groupdiff_absdiff, minimize = TRUE, range = c(-Inf, Inf)) {

      private$.measures = assert_measures(as_measures(measures))
      private$.aggfun = assert_function(aggfun)
      assert_true(all(map_chr(private$.measures, "task_type") == private$.measures[[1]]$task_type))

      if (is.null(id)) {
        id = paste0(replace_prefix(ids(measures), c(mlr_reflections$task_types$type, "fairness"), ""),
          collapse = "_")
      }

      super$initialize(
        id = sprintf("fairness.%s", assert_string(id)),
        range = range,
        properties = "requires_task",
        minimize = minimize,
        predict_type = unique(unlist(map(measures, "predict_type"))),
        task_type = private$.measures[[1]]$task_type,
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
        map_dbl(private$.measures, function(m) {
          prediction$score(m, task = task, ...)
        })
      )
    }
  )
)
