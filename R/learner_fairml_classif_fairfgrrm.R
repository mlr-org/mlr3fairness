#' @title Classification Fair Generalized Ridge Regression Learner
#' @author pfistfl
#' @name mlr_learners_classif.fairfgrrm
#' 
#' @details 
#' Fair generalized ridge regression model implemented via package `fairml`.
#' The 'unfairness' parameter is set to 0.05 as a default.
#'
#' @template class_learner
#' @templateVar id classif.fairfgrrm
#' @templateVar caller fgrrm
#'
#' @references
#' `r format_bib("scutari21")`
#'
#' @template seealso_learner
#' @template example
#' @export
LearnerClassifFairfgrrm = R6Class("LearnerClassifFairfgrrm",
  inherit = LearnerClassif,

  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      ps = ps(
        lambda = p_dbl(lower = 0, upper = Inf, tags = "train", default = 0),
        definition = p_fct(levels = c("sp-komiyama", "eo-komiyama"), default = "sp-komiyama", tags = "train"),
        save.auxiliary = p_lgl(default = FALSE, tags = "train"),
        unfairness = p_dbl(lower = 0, upper = 1, tags = "train"),
        family = p_fct(levels = c("gaussian", "binomial"), tags = "train", default = "binomial"),
        intersect = p_lgl(default = TRUE, tags = c("train", "predict"))
      )
      ps$values = list(unfairness = 0.05, intersect = FALSE)

      super$initialize(
        id = "classif.fairfgrrm",
        packages = "fairml",
        feature_types = c("integer", "numeric", "factor", "ordered"),
        predict_types = c("response", "prob"),
        properties = "twoclass",
        param_set = ps,
        man = "mlr3fairness::mlr_learners_classif.fairfgrrm"
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
      r = task$truth()

      s = get_pta(task, intersect = pars$intersect)
      s = map_dtc(s, as.numeric)
      pars = remove_named(pars, "intersect")
      p = task$data(cols = setdiff(task$feature_names, pta))
      p = int_to_numeric(p)

      mlr3misc::invoke(fairml::fgrrm, response = r, predictors = p, sensitive = s, .args = pars)
    },

    .predict = function(task) {
      # get parameters with tag "predict"
      pars = self$param_set$get_values(tags = "predict")
      pta = task$col_roles$pta
      s = get_pta(task, intersect = pars$intersect)
      s = map_dtc(s, as.numeric)
      p = task$data(cols = setdiff(self$state$feature_names, pta))
      p = int_to_numeric(p)

      if (self$predict_type == "response") {
        pred = mlr3misc::invoke(predict, self$model, new.predictors = p, new.sensitive = s, type = "class")
        list(response = drop(pred))
      } else {
        prob = mlr3misc::invoke(predict, self$model, new.predictors = p, new.sensitive = s, type = "response")
        if (length(task$class_names) == 2L) {
          prob = pprob_to_matrix(prob, task)
        } else {
          prob = prob[, , 1L]
        }
        list(prob = prob)
      }
    }
  )
)
