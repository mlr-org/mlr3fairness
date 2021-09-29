```
Feature Name: `do_something`
Start Date: 2020-07-01
Target Date: 2020-07-30
```

## Summary
[summary]: #summary

Implement the common fairness visualizations along with a clear documentation on how to interpret them in mlr3fairness packages.

## Motivation
[motivation]: #motivation

We need fairness visualizations so the package will be more user friendly. Most of the users are not expected to understand fairness problems from just the fairness metrics. They are just numbers for non-experts. However, through fairness visualizations we could provide a clear, thorough and understandable way to understand the fairness problems.

## Guide-level explanation
[guide-level-explanation]: #guide-level-explanation

There will be multiple visualization functions user could interact with. For example, the fairness compare could compare the fairness metrics on one model or between multiple models.

For visualizations about the dataset or basic measures. Users could use mlr3vis to visualize or customize their own visualizations.

Example:
```r
compas = tsk("compas")
lrn = lrn("class.rpart")
lrn$train(compas)

measures = msrs(c("fairness.tpr", "fairness.fpr"))
fairness_compare(object = lrn$predict(compas), measures, compas)
```

```r
design = benchmark_grid(
  tasks = tsk("adult_train"),
  learners = lrns(c("classif.ranger", "classif.rpart"),
                  predict_type = "prob", predict_sets = c("train", "test")),
  resamplings = rsmps("cv", folds = 3)
)

bmr = benchmark(design)
measures = msrs(c("fairness.tpr", "fairness.fpr"))
fairness_compare(object = bmr, measures)
```


## Reference-level explanation
[reference-level-explanation]: #reference-level-explanation

Internally, since we want the visualization functions to be able handle multiple type of input. Currently we think `PredictionClassif`, `BenchmarkResult` and `ResampleResult` are three most important types we need to handle. We use S3 class to implement the polymorphism of visualization class.

Example:
```r
fairness_compare <- function(object, ...){
  UseMethod("fairness_compare")
}

fairness_compare.PredictionClassif <- function(...){ #generalize visualization for Prediction Object }
fairness_compare.BenchmarkResult <- function(...){ #generalize visualization for Benchmark Object }
fairness_compare.ResampleResult <- function(...){ #generalize visualization for Resample Object (By converting to Benchmark)}
```

## Rationale, drawbacks and alternatives
[rationale-and-alternatives]: #rationale-and-alternatives

This design seems fairly obvious choice in the design space.
The main alternative to this proposal is not to implement it,
and let users to create their own plots if they need them.

## Prior art
[prior-art]: #prior-art

mlr3viz:
This is the subpackage supports all the other visualizations for mlr3.
https://github.com/mlr-org/mlr3viz

## Introduced Dependencies
This solution would introduce dependencies on the following (additional) packages:

```
ggplot2
```

## Unresolved questions
[unresolved-questions]: #unresolved-questions


* We want to add an interactive visualizations through R Shiny. But this is the future work.
* Currently those visualizations need some improvements. Like circle the anchor point in fairness - accuracy tradeoff visualizations.
