#' @title Regression Fair Regression With Covariance Constraints Learner
#' @author pfistfl
#' @details
#' Fair regression model from Zafar et al., 2019 implemented via package `fairml`.
#' The 'unfairness' parameter is set to 0.05 as a default.
#' The optimized fairness metric is statistical parity.
#'
#' @name mlr_learners_regr.fairzlm
#'
#' @template class_learner
#' @templateVar id regr.fairzlm
#' @templateVar caller zlm
#'
#' @references
#' `r format_bib("zafar19a")`
#'
#' @template seealso_learner
#' @template example
#' @export
LearnerRegrFairzlm = R6Class("LearnerRegrFairzlm",
  inherit = LearnerRegr,

  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      ps = ps(
        unfairness = p_dbl(lower = 0, upper = 1, tags = "train"),
        intersect = p_lgl(default = TRUE, tags = c("train", "predict"))
      )
      ps$values = list(unfairness = 0.05, intersect = FALSE)
      super$initialize(
        id = "regr.fairzlm",
        packages = c("fairml", "CVXR"),
        feature_types = c("integer", "numeric", "factor", "ordered"),
        predict_types = c("response"),
        param_set = ps,
        man = "mlr3fairness::mlr_learners_regr.fairzlm"
      )
    }
  ),

  private = list(

    .train = function(task) {
      assert_pta_task(task)
      # get parameters for training
      pars = self$param_set$get_values(tags = "train")

      # set column names to ensure consistency in fit and predict
      self$state$feature_names = task$feature_names

      pta = task$col_roles$pta
      r = as.numeric(task$truth())
      s = get_pta(task, intersect = pars$intersect)
      pars = remove_named(pars, "intersect")
      p = task$data(cols = setdiff(task$feature_names, pta))
      p = int_to_numeric(p)
      # use the mlr3misc::invoke function (it's similar to do.call())
      mlr3misc::invoke(fairml::zlm, response = r, sensitive = s, predictors = p, .args = pars)
    },

    .predict = function(task) {
      # get parameters with tag "predict"
      pars = self$param_set$get_values(tags = "predict")
      pta = task$col_roles$pta
      s = get_pta(task, intersect = pars$intersect)
      pars = remove_named(pars, "intersect")
      p = task$data(cols = setdiff(self$state$feature_names, pta))
      p = int_to_numeric(p)
      pred = mlr3misc::invoke(predict, self$model, new.predictors = p, .args = pars)
      list(response = pred)
    }
  )
)
