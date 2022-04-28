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
    #' Subgroup identifier.
    subgroup = NULL,

    #' @field intersect (`logical`)\cr
    #' Should groups be intersected?
    intersect = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param id (`character`)\cr
    #'   The measure's id. Set to 'fairness.<base_measure_id>' if ommited.
    #' @param base_measure (`Measure()`)\cr
    #'   The measure used to measure fairness.
    #' @param subgroup (`character`)|(`integer`)\cr
    #' Subgroup identifier. Either value for the protected attribute or position in `task$levels`.
    #' @param intersect [`logical`] \cr
    #'  Should multiple pta groups be intersected? Defaults to `TRUE`.
    #'  Only relevant if more than one `pta` columns are provided.
    initialize = function(id = NULL, base_measure, subgroup, intersect = TRUE) {
      self$base_measure = assert_measure(as_measure(base_measure))
      assert(check_character(subgroup), check_integer(subgroup))
      self$subgroup = subgroup
      self$intersect = assert_flag(intersect)

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

      groups = get_pta(task, prediction$row_ids, intersect = self$intersect)
      nms = copy(names(groups))
      # Convert to a data.table
      if (!is.data.table(self$subgroup)) {
        self$subgroup = as.data.table(setNames(list(self$subgroup), nms))
      }
      assert_subset(unlist(self$subgroup), unlist(map(groups, function(x) {unique(as.character(x))})))
      groups[, row_ids := prediction$row_ids]
      rws = intersect(prediction$row_ids, groups[self$subgroup, on = nms]$row_ids)
      prediction$clone()$filter(rws)$score(self$base_measure, task = task, ...)
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
#' @param intersect [`logical`] \cr
#'  Should multiple pta groups be intersected? Defaults to `TRUE`.
#'  Only relevant if more than one `pta` columns are provided.
#' @seealso [MeasureSubgroup]
#' @export
#' @examples
#'   t = tsk("compas")
#'   l = lrn("classif.rpart")
#'   m = groupwise_metrics(msr("classif.acc"), t)
#'   l$train(t)$predict(t)$score(m, t)
#' @return `list` \cr
#' List of [mlr3::Measure]s.
groupwise_metrics = function(base_measure, task, intersect = TRUE) {
  assert_pta_task(task)
  base_measure = assert_measure(as_measure(base_measure))
  pta = get_pta(task, rows = NULL, intersect = intersect)
  unique_groups = unlist(map(pta, function(x) as.character(unique(x))))
  map(unique_groups, MeasureSubgroup$new, base_measure = base_measure, id = NULL, intersect = intersect)
}
