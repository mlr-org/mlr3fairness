#' @title Positive Probability Measure
#' @name mlr_measures_positive_probability
#'
#' @description
#' Return the probabiliy of a positive prediction, often known as 'Calders-Wevers' gap.
#' This is defined as count of positive predictions divided by the number of observations.
#'
#' @export
#' @examplesIf rlang::is_installed("rpart")
#' library("mlr3")
#' # Create Positive Probability Measure
#' t = tsk("adult_train")
#' learner = lrn("classif.rpart", cp = .01)
#' learner$train(t)
#' measure = msr("classif.pp")
#' predictions = learner$predict(t)
#' predictions$score(measure, task = t)
MeasurePositiveProbability = R6::R6Class("MeasurePositiveProbability",
  inherit = mlr3::Measure,
  public = list(

    #' @description
    #' Initialize a Measure Positive Probability Object
    initialize = function() {
      super$initialize(
        id = "classif.pp",
        predict_type = "response",
        range = c(0, 1),
        minimize = FALSE,
        task_type = "classif"
      )
    }
  ),

  private = list(
    .score = function(prediction, task, ...) {
      mean(prediction$response == task$positive)
    }
  )
)

mlr_measures$add("classif.pp", MeasurePositiveProbability)
