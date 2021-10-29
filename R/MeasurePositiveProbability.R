#' @title Positive Probability Measure
#' @name mlr_measures_positive_probability
#'
#' @description
#' Return the positive probability of the predictions.
#' This is defined as count of positive predictions divided by the number of observations.
#'
#' @export
#' @examples
#' # Create Positive Probability Measure
#' data_task = tsk("adult_train")
#' learner = lrn("classif.rpart", cp = .01)
#' learner$train(data_task)
#' measure = msr("classif.pp")
#' predictions = learner$predict(data_task)
#' predictions$score(measure, task = data_task)
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
        minimize = FALSE)
    }
  ),

  private = list(
    .score = function(prediction, task, ...) {
      mean(prediction$response == task$positive)
    }
  )
)

mlr_measures$add("classif.pp", MeasurePositiveProbability)

