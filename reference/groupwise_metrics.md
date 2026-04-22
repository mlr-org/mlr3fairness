# Evaluate a metric on each protected subgroup in a task.

Instantiates one new measure per protected attribute group in a task.
Each metric is then evaluated only on predictions made for the given
specific subgroup.

## Usage

``` r
groupwise_metrics(base_measure, task, intersect = TRUE)
```

## Arguments

- base_measure:

  ([`Measure()`](https://mlr3.mlr-org.com/reference/Measure.html))  
  The base metric evaluated within each subgroup.

- task:

  [`mlr3::Task`](https://mlr3.mlr-org.com/reference/Task.html)  
  [`mlr3::Task()`](https://mlr3.mlr-org.com/reference/Task.html) to
  instantiate measures for.

- intersect:

  [`logical`](https://rdrr.io/r/base/logical.html)  
  Should multiple pta groups be intersected? Defaults to `TRUE`. Only
  relevant if more than one `pta` columns are provided.

## Value

`list`  
List of
[mlr3::Measure](https://mlr3.mlr-org.com/reference/Measure.html)s.

## See also

[MeasureSubgroup](https://mlr3fairness.mlr-org.com/reference/MeasureSubgroup.md)

## Examples

``` r
  library("mlr3")
  t = tsk("compas")
  l = lrn("classif.rpart")
  m = groupwise_metrics(msr("classif.acc"), t)
  l$train(t)$predict(t)$score(m, t)
#>   subgroup.acc_Male subgroup.acc_Female 
#>           0.6782069           0.6910638 
```
