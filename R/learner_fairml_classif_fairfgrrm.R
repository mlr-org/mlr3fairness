#' @title Classification Fair Generalized Ridge Regression Learner
#' @author pfistfl
#' @name mlr_learners_classif.fairfgrrm
#' 
#'  @details 
#'  Fair generalized ridge regression model implemented via package `fairml`.
#'  The 'unfairness' parameter is set to 0.05 as a default.
#'
#' @template class_learner
#' @templateVar id classif.fairfgrrm
#' @templateVar caller classif.fairfgrrm
#'
#' @references
#' <FIXME - DELETE THIS AND LINE ABOVE IF OMITTED>
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
        unfairness = p_dbl(lower = 0, upper = 1, default = NULL),
        family = p_fct(levels = c("gaussian", "binomial"), tags = "train", default = "binomial")
      )
      ps$values = list(unfairness = 0.05)

      super$initialize(
        id = "classif.fairfgrrm",
        packages = "fairml",
        feature_types = c("integer", "numeric", "factor"),
        predict_types = c("response"),
        param_set = ps,
        man = "mlr3extralearners::mlr_learners_classif.fairfgrrm"
      )
    }
  ),

  private = list(

    .train = function(task) {
      # get parameters for training
      pars = self$param_set$get_values(tags = "train")

      # set column names to ensure consistency in fit and predict
      self$state$feature_names = task$feature_names

      predictors = task$data(cols = setdiff(task$feature_names, task$col_roles$pta))
      sensitive = task$data(cols = task$col_roles$pta)
      response = task$data(cols = task$target_names)
      mlr3misc::invoke(fairml::classif.fairfgrrm, response = response, predictors = predictors, sensitive = sensitive, .args = pars)
    },

    .predict = function(task) {
      # get parameters with tag "predict"
      pars = self$param_set$get_values(tags = "predict")

      pta = task$col_roles$pta
      s = task$data(cols = pta)[[1]]
      p = task$data(cols = setdiff(self$state$feature_names, pta))

      pred = mlr3misc::invoke(predict, self$model, new.predictors = p, new.sensitive = s, type = type, .args = pars)
    }
  )
)
