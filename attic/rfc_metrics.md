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

I propose when applying a metric method, what occurs should be ...

Example:
```r
#Initialize the data task
adult_train = tsk("adult_train")
adult_test = tsk("adult_test")

#Train the model and make the prediction
learner = lrn("classif.rpart", cp = .01)
learner$train(adult_train)
predictions = learner$predict(adult_test)

#Create the Fairness Metrics, in this example. I want to measure the False Positive Rate Bias
measure = MeasureFairnessSimple$new(msr("classif.fpr"))
fprb = predictions$score(measure, adult_test, operation = "abs_diff")

```

## Reference-level explanation
[reference-level-explanation]: #reference-level-explanation

Internally, the function would look the following:

Example:
```r
MeasureFairnessSimple = R6Class(
inherits = "Measure",
initialize = function(measure) {
  self$measure = measure
  # here do some other thing setting task types etc.
  # minimize, properties, ...
  self$minimize = measure$minimize
}
...

$.score = function(task, operation, ...) {
m1 = self$measure$score(group1, )
m2 = self$measure$score....

if (opertation = "quotient) m1 /m2
else if(operation = "abs_diff") abs(m1-m2)
else if(operation = "diff") (m1-m2)
...
```

## Rationale, drawbacks and alternatives
[rationale-and-alternatives]: #rationale-and-alternatives

This design seems fairly obvious choice in the design space.
The main alternative to this proposal is not to implement it,
and let users to calculate joined subslices from indexes or pointers.

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
    R (>= 3.1.0)
Imports:
    checkmate,
    PRROC
```
Using this package would allow us to use some basic fairness metrics instead of re-implementing and maintining
N loc ourselves.


## Unresolved questions
[unresolved-questions]: #unresolved-questions
