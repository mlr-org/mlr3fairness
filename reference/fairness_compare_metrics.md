# Compare different metrics

Compare learners with respect to to one or multiple metrics. Metrics can
but be but are not limited to fairness metrics.

## Usage

``` r
compare_metrics(object, ...)
```

## Arguments

- object:

  ([mlr3::PredictionClassif](https://mlr3.mlr-org.com/reference/PredictionClassif.html)
  \|
  [mlr3::BenchmarkResult](https://mlr3.mlr-org.com/reference/BenchmarkResult.html)
  \|
  [mlr3::ResampleResult](https://mlr3.mlr-org.com/reference/ResampleResult.html))  
  The object to create a plot for.

  - If provided a
    ([mlr3::PredictionClassif](https://mlr3.mlr-org.com/reference/PredictionClassif.html)).
    Then the visualization will compare the fairness metrics among the
    binary level from protected field through bar plots.

  - If provided a
    ([mlr3::ResampleResult](https://mlr3.mlr-org.com/reference/ResampleResult.html)).
    Then the visualization will generate the boxplots for fairness
    metrics, and compare them among the binary level from protected
    field.

  - If provided a
    ([mlr3::BenchmarkResult](https://mlr3.mlr-org.com/reference/BenchmarkResult.html)).
    Then the visualization will generate the boxplots for fairness
    metrics, and compare them among both the binary level from protected
    field and the models implemented.

- ...:

  The arguments to be passed to methods, such as:

  - `fairness_measures` (list of
    [mlr3::Measure](https://mlr3.mlr-org.com/reference/Measure.html))  
    The fairness measures that will evaluated on object, could be single
    [mlr3::Measure](https://mlr3.mlr-org.com/reference/Measure.html) or
    list of
    [mlr3::Measure](https://mlr3.mlr-org.com/reference/Measure.html)s.
    Default measure set to be `msr("fairness.acc")`.

  - `task`
    ([mlr3::TaskClassif](https://mlr3.mlr-org.com/reference/TaskClassif.html))  
    The data task that contains the protected column, only required when
    object is
    ([mlr3::PredictionClassif](https://mlr3.mlr-org.com/reference/PredictionClassif.html)).

## Value

A 'ggplot2' object.

## Protected Attributes

The protected attribute is specified as a `col_role` in the
corresponding
[`mlr3::Task()`](https://mlr3.mlr-org.com/reference/Task.html):  
`<Task>$col_roles$pta = "name_of_attribute"`  
This also allows specifying more than one protected attribute, in which
case fairness will be considered on the level of intersecting groups
defined by all columns selected as a predicted attribute.

## Examples

``` r
library("mlr3")
library("mlr3learners")

# Setup the Fairness Measures and tasks
task = tsk("adult_train")$filter(1:500)
learner = lrn("classif.ranger", predict_type = "prob")
learner$train(task)
predictions = learner$predict(task)
design = benchmark_grid(
  tasks = task,
  learners = lrns(c("classif.ranger", "classif.rpart"),
    predict_type = "prob", predict_sets = c("train", "test")),
  resamplings = rsmps("cv", folds = 3)
)

bmr = benchmark(design)
fairness_measure = msr("fairness.tpr")
fairness_measures = msrs(c("fairness.tpr", "fairness.fnr", "fairness.acc"))

# Predictions
compare_metrics(predictions, fairness_measure, task)

compare_metrics(predictions, fairness_measures, task)


# BenchmarkResult and ResamplingResult
compare_metrics(bmr, fairness_measure)

compare_metrics(bmr, fairness_measures)
```
