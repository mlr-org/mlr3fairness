#' @title Positive Probability Measure
#' @name mlr_measures_positive_probability
#'
#' @description
#' Return the positive probability of the predictions. Formula: P(positive prediction) =
#' positive_predictions / all_predictions
#'
#' @export
#' @examples
#' # Create Positive Probability Measure
#' library(mlr3)
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
        packages = character(),
        properties = character(),
        predict_type = "response",
        range = c(0, 1),
        minimize = FALSE)
    }
  ),

  private = list(
    .score = function(prediction, task, ...) {
      pp = function(response, positive) {
        positive_count = sum(response == positive)
        return(positive_count / length(response))
      }
      pp(prediction$response, task$positive)
    }
  )
)

mlr_measures$add("classif.pp", MeasurePositiveProbability)
