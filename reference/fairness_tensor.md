# Compute the Fairness Tensor given a Prediction and a Task

A fairness tensor is a list of groupwise confusion matrices.

## Usage

``` r
fairness_tensor(object, normalize = "all", ...)

# S3 method for class 'data.table'
fairness_tensor(object, normalize = "all", task, ...)

# S3 method for class 'PredictionClassif'
fairness_tensor(object, normalize = "all", task, ...)

# S3 method for class 'ResampleResult'
fairness_tensor(object, normalize = "all", ...)
```

## Arguments

- object:

  ([`data.table::data.table()`](https://rdrr.io/pkg/data.table/man/data.table.html)
  \|
  [mlr3::PredictionClassif](https://mlr3.mlr-org.com/reference/PredictionClassif.html)
  \|
  [mlr3::ResampleResult](https://mlr3.mlr-org.com/reference/ResampleResult.html))  
  A data.table with columns `truth` and `prediction`, a
  [mlr3::PredictionClassif](https://mlr3.mlr-org.com/reference/PredictionClassif.html)
  or a
  [mlr3::ResampleResult](https://mlr3.mlr-org.com/reference/ResampleResult.html).

- normalize:

  (`character`)  
  How should the fairness tensor be normalized? "all" normalizes entries
  by dividing by dataset size, "group" normalizes entries by dividing by
  group size and "none" does not conduct any normalization at all.

- ...:

  `any`  
  Currently not used.

- task:

  ([mlr3::TaskClassif](https://mlr3.mlr-org.com/reference/TaskClassif.html))  
  A
  [mlr3::TaskClassif](https://mlr3.mlr-org.com/reference/TaskClassif.html).
  Needs `col_role` `"pta"` to be set.

## Value

[`list()`](https://rdrr.io/r/base/list.html) of confusion matrix for
every group in `"pta"`.

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
task = tsk("compas")
prediction = lrn("classif.rpart")$train(task)$predict(task)
fairness_tensor(prediction, task = task)
#> $Male
#>         truth
#> response         0         1
#>        0 0.3016850 0.1407971
#>        1 0.1197343 0.2474076
#> 
#> $Female
#>         truth
#> response          0          1
#>        0 0.09332469 0.02867790
#>        1 0.03013610 0.03823720
#> 
```
