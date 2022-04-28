#' @title Regression Fair Ridge Regression Learner
#' 
#' @details 
#' Fair ridge regression learner implemented via package `fairml`.
#' The 'unfairness' parameter has been initialized to 0.05.
#' @template intersect
#'
#' @author pfistfl
#' @name mlr_learners_regr.fairfrrm
#'
#' @template class_learner
#' @templateVar id regr.fairfrrm
#' @templateVar caller frrm
#'
#' @references
#' `r format_bib("scutari21")`
#'
#' @template seealso_learner
#' @template example
#' @export
LearnerRegrFairfrrm = R6Class("LearnerRegrFairfrrm",
  inherit = LearnerRegr,

  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      ps = ps(
        lambda = p_dbl(lower = 0, upper = Inf, tags = "train", default = 0),
        definition = p_fct(levels = c("sp-komiyama", "eo-komiyama"), default = "sp-komiyama", tags = "train"),
        save.auxiliary = p_lgl(default = FALSE, tags = "train"),
        unfairness = p_dbl(lower = 0, upper = 1, tags = "train")
      )
      ps$values = list(unfairness = .05)
      super$initialize(
        id = "regr.fairfrrm",
        packages = "fairml",
        feature_types = c("integer", "numeric", "factor", "ordered"),
        predict_types = c("response"),
        param_set = ps,
        man = "mlr3fairness::mlr_learners_regr.fairfrrm"
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
      s = get_pta(task, intersect = FALSE)
      p = task$data(cols = setdiff(task$feature_names, pta))
      p = int_to_numeric(p)
      mlr3misc::invoke(fairml::frrm, response = r, predictors = p, sensitive = s, .args = pars)
    },

    .predict = function(task) {
      # get parameters with tag "predict"
      pars = self$param_set$get_values(tags = "predict")
      pta = task$col_roles$pta
      s = get_pta(task, intersect = FALSE)
      p = task$data(cols = setdiff(self$state$feature_names, pta))
      p = int_to_numeric(p)
      pred = mlr3misc::invoke(predict, self$model, new.predictors = p, new.sensitive = s, .args = pars)
      list(response = pred)
    }
  )
)
