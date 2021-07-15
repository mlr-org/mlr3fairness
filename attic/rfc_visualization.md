```
Feature Name: `do_something`
Start Date: 2019-11-04
Target Date:
```

## Summary
[summary]: #summary

Implement the common fairness visualizations along with a clear documentation on how to interpret them in mlr3fairness packages.

## Motivation
[motivation]: #motivation

We need fairness visualizations so the package will be more user friendly. The visualizations are much more clear than fairness metrics. Moreover, users could use visualizations to demonstrate the fairness problems to non-experts.

## Guide-level explanation
[guide-level-explanation]: #guide-level-explanation

Users could use the following code to display the fairness metrics.
* Couple questions need to resolve
* Extend the current autoplot function in mlr3viz or create new autoplot function?
* Extend the current autoplot function for fairness comparison.
* Using percentage is better for visualization. (Existing autoplot function, solutions: Preprocess the datatasks)

```r
library(mlr3)
library(mlr3fairness)
library(mlr3viz)
library(ggplot2)
data <- tsk("compas")
learner = lrn("classif.rpart")
learner$train(data)
```

For datatasks:
Example1:
```r
data <- tsk("compas")
autoplot(data) + facet_wrap(~race)
```

Example2:
```r
library(ggplot2)
data <- tsk("compas")
autoplot(data)
```

For Metrics:
```
autoplot(data, learner = learner, metrics = msrs("classif.fpr"))
autoplot(data, learner = lrns(learner1, learner2), metrics = msrs("classif.fpr"))
```

For model:
```r




#plot(fairness_pca(fobject))
autoplot(data, learner = learner, type = "pca", metrics = msrs(...))

#plot(fairness_heatmap(fobject))
autoplot(data, learner = learner, type = "heatmap", metrics = msrs(...))

#plot(fairness_radar(fobject))
autoplot(data, learner = learner, type = "radar", metrics = msrs(...))
```


## Reference-level explanation
[reference-level-explanation]: #reference-level-explanation

Internally, the function would look the following:

Example:
```r
do_something = function(a, b) {
  ...
}
```


## Rationale, drawbacks and alternatives
[rationale-and-alternatives]: #rationale-and-alternatives

This design seems fairly obvious choice in the design space.
The main alternative to this proposal is not to implement it,
and let users to calculate joined subslices from indexes or pointers.

## Prior art
[prior-art]: #prior-art

There exists a function that implements the API as here...

## Introduced Dependencies
This solution would introduce dependencies on the following (additional) packages:

Those packages either depend on or import the following other (additional) packages:

Using this package would allow us to ... instead of re-implementing and maintining
N loc ourselves.


## Unresolved questions
[unresolved-questions]: #unresolved-questions
