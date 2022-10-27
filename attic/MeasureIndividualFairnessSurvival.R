
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
