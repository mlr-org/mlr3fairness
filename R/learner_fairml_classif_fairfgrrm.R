#' @title Classification Fair Generalized Ridge Regression Learner
#' @author pfistfl
#' @name mlr_learners_classif.fairfgrrm
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
      # FIXME - MANUALLY ADD PARAM_SET BELOW AND THEN DELETE THIS LINE
      ps = <param_set>

      # FIXME - MANUALLY UPDATE PARAM VALUES BELOW IF APPLICABLE THEN DELETE THIS LINE.
      # OTHERWISE DELETE THIS AND LINE BELOW.
      ps$values = list(<param_vals>)

      super$initialize(
        id = "classif.fairfgrrm",
        packages = "fairml",
        feature_types = c("integer", "numeric"),
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

      # FIXME - If learner does not have 'weights' property then delete these lines.
      if ("weights" %in% task$properties) {
        pars = insert_named(pars, list(weights = task$weights$weight))
      }

      # FIXME - <Create objects for the train call>
      # <At least "data" and "formula" are required>
      formula = task$formula()
      data = task$data()

      # FIXME - <here is space for some custom adjustments before proceeding to the
      # train call. Check other learners for what can be done here>

      # use the mlr3misc::invoke function (it's similar to do.call())
      mlr3misc::invoke(fairml::classif.fairfgrrm,
                       formula = formula,
                       data = data,
                       .args = pars)
    },

    .predict = function(task) {
      # get parameters with tag "predict"
      pars = self$param_set$get_values(tags = "predict")

      # get newdata and ensure same ordering in train and predict
      newdata = task$data(cols = self$state$feature_names)

      pred = mlr3misc::invoke(predict, self$model, newdata = newdata,
                              type = type, .args = pars)

      # FIXME - ADD PREDICTIONS TO LIST BELOW
      list(...)
    }
  )
)

.extralrns_dict$add("classif.fairfgrrm", LearnerClassifFairfgrrm)
