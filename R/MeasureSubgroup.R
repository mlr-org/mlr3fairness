#' @title Evaluate a metric on a subgroup
#'
#' @description
#' Allows for calculation of arbitrary [mlr3::Measure()]s on a selected sub-group.
#'
#' @seealso [MeasureFairness], [groupwise_metrics]
#' @export
#' @examples
#' # Create MeasureFairness to measure the Predictive Parity.
#' t = tsk("adult_train")
#' learner = lrn("classif.rpart", cp = .01)
#' learner$train(t)
#' measure = msr("subgroup", base_measure = msr("classif.acc"), subgroup = "Female")
#' predictions = learner$predict(t)
#' predictions$score(measure, task = t)
MeasureSubgroup = R6::R6Class("MeasureSubgroup", inherit = Measure,
  public = list(
    #' @template field_base_measure
    base_measure = NULL,

    #' @field subgroup (`character`)|(`integer`)\cr
    #' Subgroup identifier. Either value for the protected attribute or position in `task$levels`.
    subgroup = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param id (`character`)\cr
    #'   The measure's id. Set to 'fairness.<base_measure_id>' if ommited.
    #' @param base_measure (`Measure()`)\cr
    #'   The measure used to measure fairness.
    #' @param subgroup (`character`)|(`integer`)\cr
    #' Subgroup identifier. Either value for the protected attribute or position in `task$levels`.
    initialize = function(id = NULL, base_measure, subgroup) {
      self$base_measure = assert_measure(as_measure(base_measure))
      assert(check_character(subgroup), check_integer(subgroup))
      self$subgroup = subgroup

      if (is.null(id)) {
        id = replace_prefix(base_measure$id, mlr_reflections$task_types$type, "subgroup.")
        id = paste(id, self$subgroup, sep = "_")
      }
      super$initialize(
        id = id,
        range = self$base_measure$range,
        task_type = self$base_measure$task_type,
        properties = "requires_task",
        minimize = self$base_measure$minimize,
        predict_type = self$base_measure$predict_type,
        packages = "mlr3fairness",
        man = "mlr_measures_subgroup"
      )
    }
  ),

  private = list(
    .score = function(prediction, task, ...) {
      assert_pta_task(task)
      if (is.integer(self$subgroup)) {
        self$subgroup = task$levels(cols = task$col_roles$pta)[[1]][self$subgroup]
      }
      groups = task$data(cols = task$col_roles$pta, rows = prediction$row_ids)[[1]]
      assert_choice(self$subgroup, levels(groups))
      rws = prediction$row_ids[groups == self$subgroup]
      prediction$clone()$filter(rws)$score(self$base_measure, task = task)
    }
  )
)

#' @title Evaluate a metric on each protected subgroup in a task.
#'
#' @description
#' Instantiates one new measure per protected attribute group in a task.
#' Each metric is then evaluated only on predictions made for the given specific subgroup.
#'
#' @template param_base_measure
#' @param task [`Task`] \cr
#'   [mlr3::Task()] to instantiate measures for.
#' @seealso [MeasureSubgroup]
#' @export
#' @examples
#'   t = tsk("compas")
#'   l = lrn("classif.rpart")
#'   m = groupwise_metrics(msr("classif.acc"), t)
#'   l$train(t)$predict(t)$score(m, t)
#' @return `list` \cr
#' List of [mlr3::Measure]s.
groupwise_metrics = function(base_measure, task) {
  assert_pta_task(task)
  base_measure = assert_measure(as_measure(base_measure))
  map(task$levels(cols = task$col_roles$pta)[[1]], MeasureSubgroup$new, base_measure = base_measure, id = NULL)
}