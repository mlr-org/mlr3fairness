#' @title Individual Fairness
#'
#' @description
#' This measure [mlr3::Measure()] allows for computing summary individual fairness statistics.
#' See [individual_fairness] for computing individual level fairness scores.
#'
#' Implements a fairness notion roughly requiring that "similar individuals should be treated similarly".
#' Mathematically, computing individual fairness requires defining two distance metrics \eqn{d_x}{dX} and \eqn{d_y}{dY}
#' defined on feature and prediction spaces respectively. 
#' \deqn{
#'    d_y(f(x1), f(x2)) \leq d_x(x1, x2)
#' }{
#'    dY(f(x1), f(x2) <= dX(f1, x2))
#' }
#'
#' This is computed by comparing differences between predictions and differences between features, be default using the
#' Gower distance. The user is encouraged to adapt the distance metric and features used for computation to form a
#' relevant distance metric that encodes ethical or regulatory considerations.
#' 
#' @details 
#' Both dX and dY need to be functions that take as input arguments `x`, `y` (two data.tables) and 
#' an argument `cols` which specifies the colnames to be used for computing distances.
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

    #' @field fraction [`function`]
    #' Fraction of points to use for distance computation. 
    #' Set to smaller values for larger datasets.
    fraction = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param id (`character`)\cr
    #'   The measure's id. Set to 'fairness.individual' if ommited.
    #' @param task_type [`character] Task type to construct the metric for.
    #' @param predict_type [`character``] Predict type to construct the metric for.
    #' @param dX [`function] distance metric for features. Defaults to `gower::gower_dist`. See `details` for additional info.
    #' @param dY [`function] distance metric for predictions. Defaults to `gower::gower_dist`.  See `details` for additional info.
    #' @param fraction [`numeric] For large datasets: Should computation be approximated by computing on only a fraction? Initialized to 1.
    initialize = function(id = "fairness.individual", task_type, predict_type, dX = NULL, dY = NULL, fraction = 1) {
      self$dX = assert_function(dX, null.ok = TRUE)
      self$dY = assert_function(dY, null.ok = TRUE)
      self$fraction = assert_number(fraction, lower = 0, upper = 1)

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
        dt = individual_fairness(task$data(rows = prediction$row_ids), prediction, self$dX, self$dY, self$fraction)
        mean(dt$individual_fairness)
    }
  )
)

mlr_measures$add("fairness.avg_individual", MeasureAvgIndividualFairness)


#' @title Group Fairness for Survival
#'
#' @description
#' This measure [mlr3::Measure()] allows for computing summary fairness statistics from survival data.
#' See [individual_fairness] for computing individual level fairness scores.
#'
#' Implements a fairness notion roughly requiring that "similar individuals should be treated similarly".
#' Mathematically, computing individual fairness requires defining two distance metrics \eqn{d_x}{dX} and \eqn{d_y}{dY}
#' defined on feature and prediction spaces respectively. 
#' \deqn{
#'    d_y(f(x1), f(x2)) \leq d_x(x1, x2)
#' }{
#'    dY(f(x1), f(x2) <= dX(f1, x2))
#' }
#'
#' This is computed by comparing differences between predictions and differences between features, be default using the
#' Gower distance. The user is encouraged to adapt the distance metric and features used for computation to form a
#' relevant distance metric that encodes ethical or regulatory considerations.
#' 
#' @details 
#' Both dX and dY need to be functions that take as input arguments `x`, `y` (two data.tables) and 
#' an argument `cols` which specifies the colnames to be used for computing distances.
#' 
#' 
#' @export
#' @examples
#' # Create MeasureFairness to measure survival group fairness
#'  m = msr("fairness.surv_group")
#'  t = tsk("rats")
#'  t$col_roles$pta = "sex"
#'  l = lrn("surv.coxph")
#'  l$train(t)
#'  l$predict(t)$score(m, t)
MeasureSurvGroupFairness = R6::R6Class("MeasureSurvGroupFairness", inherit = Measure,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param id (`character`)\cr
    #'   The measure's id. Set to 'fairness.individual' if ommited.
    initialize = function(id = "fairness.survgroup") {
      super$initialize(
        id = id,
        range = c(0,1),
        task_type = "surv",
        properties = "requires_task",
        minimize = TRUE,
        predict_type = "lp",
        packages = c("mlr3fairness", "mlr3proba"),
        man = "mlr_measures_fairness"
      )
    }
  ),

  private = list(
    .score = function(prediction, task, ...) {
      assert_pta_task(task)
      pta = task$col_roles$pta
      dt = task$data(rows = prediction$row_ids, cols = pta)
      dt[, lp := prediction$lp]
      mn = mean(dt$lp)
      # Implementing https://github.com/kkeya1/FairSurv/blob/49ba2f925f81e3fdbe22558961568d809ec5bb83/fairness_measures.py#L27
      dt2 = dt[, .(sum_lp = sum(lp), .N), by = pta]
      dt2 = dt2[, .(mean_lp = (sum_lp + 0.5) / (N + 1))]
      return(max(dt2$mean_lp - mn))
    }
  )
)

mlr_measures$add("fairness.surv_group", MeasureSurvGroupFairness)
