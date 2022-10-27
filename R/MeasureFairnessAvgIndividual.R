#' @title Individual Fairness Metrics
#'
#' @description
#' This measure [mlr3::Measure()] allows for computing summary individual fairness statistics.
#' See [individual_fairness] for computing individual level fairness scores.
#'
#' Implements a fairness notion roughly requiring that "similar individuals should be treated similarly".
#' Mathematically, computing individual fairness requires defining two distance metrics \eqn{d_x}{dX} and \eqn{d_y}{dY}
#' defined on feature and prediction spaces respectively.
#' \deqn{
#'    d_y(f(x1), f(x2)) \leq L \cdot d_x(x1, x2)
#' }{
#'    dY(f(x1), f(x2) <= L * dX(f1, x2))
#' }
#'
#' This is computed by comparing differences between predictions and differences between features, be default using the
#' Gower distance. The user is encouraged to adapt the distance metric and features used for computation to form a
#' relevant distance metric that encodes ethical or regulatory considerations.
#' 
#' @details 
#' Both dX and dY need to be functions that take as input arguments `i`, `j` (two data.tables) and
#' compute the element-wise distance between each row from `i` and `j`.
#' If dX or dY are not provided, they both default to the Gower distance [`gower::gower_dist`].
#' dX can have an additional argument `cols` which specifies the colnames to be used for computing distances.
#' 
#' @references
#' `r format_bib("dwork")`
#' `r format_bib("mukherjee")`
#' 
#' @export
#' @examples
#' # Create MeasureFairness to measure the Predictive Parity.
#' t = tsk("adult_train")
#' learner = lrn("classif.rpart", cp = .01)
#' learner$train(t)
#' measure = msr("fairness", base_measure = msr("classif.ppv"))
#' predictions = learner$predict(t)
#' predictions$score(measure, task = t)
MeasureAvgIndividualFairness = R6::R6Class("MeasureAvgIndividualFairness", inherit = Measure,
  public = list(
      
    #' @field dX [`function`]
    #' Distance function between observations.
    dX = NULL,

    
    #' @field dY [`function`]
    #' Distance function between predictions.
    dY = NULL,

    #' @field cols [`function`]
    #' Columns to use for computing the distance in dX.
    cols = NULL,

    #' @field fraction [`function`]
    #' Fraction of points to use for distance computation. 
    #' Set to smaller values for larger datasets.
    fraction = NULL,


    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param id (`character`)\cr
    #'   The measure's id. Set to 'fairness.individual' if omitted.
    #' @param task_type [`character] Task type to construct the metric for.
    #' @param predict_type [`character``] Predict type to construct the metric for.
    #' @param dX [`function] distance metric for features. Defaults to `gower::gower_dist`. See `details` for additional info.
    #' @param dY [`function] distance metric for predictions. Defaults to `gower::gower_dist`.  See `details` for additional info.
    #' @param fraction [`numeric] For large datasets: Should computation be approximated by computing on only a fraction? Initialized to 1.
    #' @param cols [`character`] The columns to use for computing distances in dX. Need to be a subset of columns available from the data.
    #'        If not specified, all features available in the task except for features listed as a protected attribute (via `col_role` `pta` are used.
    initialize = function(id = "fairness.individual", task_type, predict_type, dX = NULL, dY = NULL, fraction = 1, cols = NULL) {
      self$dX = assert_function(dX, null.ok = TRUE)
      self$dY = assert_function(dY, null.ok = TRUE)
      self$fraction = assert_number(fraction, lower = 0, upper = 1)
      self$cols = assert_character(cols, null.ok = TRUE)

      super$initialize(
        id = id,
        range = c(0,1),
        task_type = task_type,
        properties = "requires_task",
        minimize = TRUE,
        predict_type = predict_type,
        packages = "mlr3fairness",
        man = "mlr_measures_fairness"
      )
    }
  ),

  private = list(
    .score = function(prediction, task, ...) {
        cols = self$cols %??% setdiff(task$col_roles$feature, task$col_roles$pta)
        dt = individual_fairness(task$data(rows = prediction$row_ids, cols = cols), prediction, self$dX, self$dY, self$fraction)
        mean(dt$individual_fairness)
    }
  )
)

mlr_measures$add("fairness.avg_individual", MeasureAvgIndividualFairness)
