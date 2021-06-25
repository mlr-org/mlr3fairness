```
Feature Name: `Fairness Metrics`
Start Date: 2021-06-15
Target Date: 2021-06-22
```

## Summary
[summary]: #summary

Add fairness related metrics to fairness package.

## Motivation
[motivation]: #motivation
Those metrics could be used to evaluate the bias and accuracy of the dataset. Then further being used in debiasing algorithms and visualizations. Both of them would be the main functions of this package.

## Guide-level explanation
[guide-level-explanation]: #guide-level-explanation

I propose when users need to aseess some groupwise metrics. Most likely to be some fairness metrics.

In the following examples, we want to assess the False Positive Rate Bias of Classification Model between Male and Female on Adult Dataset.

What occurs should be ...

Example:
```r
# Initialize the data task and learner task. 

learner = lrn("classif.rpart", cp = .01)
adult_train = tsk("adult_train")
adult_test = tsk("adult_test")

#adult_train$col_roles --> $pta [1] "sex"
adult_train$col_roles
learner$train(adult_train)

#Create the fairness measure and its base measure
me = MeasureFairness$new("groupwise_abs_diff", msr("classif.fpr"))

#Get the fairness measure
predictions = learner$predict(adult_test)
predictions$score(me, task = adult_test)
>>>fairness.groupwise_abs_diff 
                  0.0767116 
```

## Reference-level explanation
[reference-level-explanation]: #reference-level-explanation

Internally, the function would look the following:

Example:
```r
MeasureFairness = R6Class("MeasureFairness", inherit = Measure, cloneable = FALSE,
  public = list(
    #' @template field_fun
    fun = NULL,

    #' @template field_base_measure
    base_measure = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #' @param name The name of the fairnessMeasure (Will drop later)
    #' @param base_measure ([base_measure])
    initialize = function(base_measure) {
      info = mlr3fairness::measures[["groupwise_abs_diff"]]
      super$initialize(
        id = paste0("fairness.", base_measure$id),
        range = c(info$lower, info$upper),
        minimize = info$minimize,
        predict_type = base_measure$predict_type,
        packages = "mlr3fairness",
        man = paste0("mlr_measures_fairness.", base_measure$id),
        properties = info$properties
      )
      self$fun = get(name, envir = asNamespace("mlr3fairness"), mode = "function")
      self$base_measure = base_measure
    }
  ),

  private = list(
    .score = function(prediction, task, ...) {
      assert_prediction(prediction)
      print(task$man)
      if ("requires_task" %in% self$properties && is.null(task)) {
        stopf("Measure '%s' requires a task", self$id)
      }

      invoke(self$fun, prediction = prediction, na_value = self$na_value, data_task = task,
           base_measure = self$base_measure)
    }
  )
)


mlr_measures$add("fairness.groupwise_abs_diff", MeasureFairness, name = "groupwise_abs_diff")
```

The groupwise operations are performed inside the Fairness Metric functions. See below:
```r
groupwise_abs_diff <- function(prediction, base_measure, positive, data_task, response = NULL, ...){
  #Assert the status for all the parameters

  subcol = data_task$col_roles$pta
  prediction = split(as.data.table(prediction), data_task$data(cols = subcol))
  prediction = lapply(prediction, as_prediction_classif)
  msr1 = prediction[[1]]$score(base_measure)
  msr2 = prediction[[2]]$score(base_measure)
  return(abs(msr1 - msr2))
}

#' @include measures.R
add_measure(groupwise_abs_diff, "Groupwise Absolute Difference", "regr", 0, Inf, FALSE)

```

## Rationale, drawbacks and alternatives
[rationale-and-alternatives]: #rationale-and-alternatives

### Rationale
The rationale for this implementation is rather simple. We need to implement multiple fairness measures. This MeasureFairness class would be the most basic infrustructures for those metrics.

### Drawbacks
* For the current implementation, we need to include the base measures from mlr3measures. This introduce some dependency on other packages.
* The final design principles are not settled yet, so we cannot determine the main drawbacks. But for the following we will talk about those drawbacks.

### alternatives
There are two type of design for the fairness measure class. The first design would be the MeasureFairness Class shown above. The second design principle would be move the fairness operation to be inside the MeasureClass. The following examples could be more clear:

First Design:
* We could implement each measure but this would need maintenance overhead
* This will need more documentations and examples intend for user to understand how to use them.
```r
me = MeasureFairness$new("groupwise_abs_diff", msr("classif.fpr"))

predictions = learner$predict(adult_test)
predictions$score(me, task = adult_test)
>>>fairness.groupwise_abs_diff 
                  0.0767116 
```

Second Design:
* This might add difficulty to the users if they want to perform more complex operations.
* It will also make the MetricFariness class more complex, we could potentially solve this problem by adding MetricFairnessComplex or other class to handle those operations. However, it will again need more documentation and mainteinance problems.
```r
me = MeasureFairness$new(msr("classif.fpr"))

#Get the fairness measure
predictions = learner$predict(adult_test)
predictions$score(me, task = adult_test, ops = "groupwise_abs_diff)
>>>fairness.groupwise_abs_diff 
                  0.0767116 
```

## Prior art
[prior-art]: #prior-art

There exists a function that implements the API as here...
https://github.com/mlr-org/mlr3measures/blob/main/R/binary_fn.R

## Introduced Dependencies
This solution would introduce dependencies on the following (additional) packages:

mlr3measures

Those packages either depend on or import the following other (additional) packages:
```
Depends:
    R (>= 3.5.0)
Imports:
    mlr3,
    checkmate,
    paradox,
    R6 (>= 2.4.1),
    data.table (>= 1.13.6),
    mlr3measures (>= 0.3.0)
```
Using this package would allow us to use some basic fairness metrics instead of re-implementing and maintining
N loc ourselves.


## Unresolved questions
[unresolved-questions]: #unresolved-questions
