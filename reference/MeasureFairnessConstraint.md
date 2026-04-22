# Fairness Constraint Measure

This measure allows constructing for 'constraint' measures of the
following form:  
\$\$min performance subject to fairness \< \epsilon\$\$

## Protected Attributes

The protected attribute is specified as a `col_role` in the
corresponding
[`mlr3::Task()`](https://mlr3.mlr-org.com/reference/Task.html):  
`<Task>$col_roles$pta = "name_of_attribute"`  
This also allows specifying more than one protected attribute, in which
case fairness will be considered on the level of intersecting groups
defined by all columns selected as a predicted attribute.

## See also

mlr_measures_fairness

## Super class

[`mlr3::Measure`](https://mlr3.mlr-org.com/reference/Measure.html) -\>
`MeasureFairnessConstraint`

## Public fields

- `performance_measure`:

  ([`Measure()`](https://mlr3.mlr-org.com/reference/Measure.html))  
  The performance measure to be used.

- `fairness_measure`:

  ([`Measure()`](https://mlr3.mlr-org.com/reference/Measure.html))  
  The fairness measure to be used.

- `epsilon`:

  (`numeric`)  
  Deviation from perfect fairness that is allowed.

## Methods

### Public methods

- [`MeasureFairnessConstraint$new()`](#method-MeasureFairnessConstraint-new)

- [`MeasureFairnessConstraint$clone()`](#method-MeasureFairnessConstraint-clone)

Inherited methods

- [`mlr3::Measure$aggregate()`](https://mlr3.mlr-org.com/reference/Measure.html#method-aggregate)
- [`mlr3::Measure$format()`](https://mlr3.mlr-org.com/reference/Measure.html#method-format)
- [`mlr3::Measure$help()`](https://mlr3.mlr-org.com/reference/Measure.html#method-help)
- [`mlr3::Measure$print()`](https://mlr3.mlr-org.com/reference/Measure.html#method-print)
- [`mlr3::Measure$score()`](https://mlr3.mlr-org.com/reference/Measure.html#method-score)

------------------------------------------------------------------------

### Method `new()`

Creates a new instance of this
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

#### Usage

    MeasureFairnessConstraint$new(
      id = NULL,
      performance_measure,
      fairness_measure,
      epsilon = 0.01,
      range = c(-Inf, Inf)
    )

#### Arguments

- `id`:

  (`character`)  
  The measure's id. Set to 'fairness.\<base_measure_id\>' if ommited.

- `performance_measure`:

  ([`Measure()`](https://mlr3.mlr-org.com/reference/Measure.html))  
  The measure used to measure performance (e.g. accuracy).

- `fairness_measure`:

  ([`Measure()`](https://mlr3.mlr-org.com/reference/Measure.html))  
  The measure used to measure fairness (e.g. equalized odds).

- `epsilon`:

  (`numeric`)  
  Allowed divergence from perfect fairness. Initialized to 0.01.

- `range`:

  (`numeric`)  
  Range of the resulting measure. Defaults to `c(-Inf, Inf)`.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    MeasureFairnessConstraint$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
# Accuracy subject to equalized odds fairness constraint:
library("mlr3")
t = tsk("adult_train")
learner = lrn("classif.rpart", cp = .01)
learner$train(t)
measure = msr("fairness.constraint", id = "acc_tpr", msr("classif.acc"), msr("fairness.tpr"))
predictions = learner$predict(t)
predictions$score(measure, task = t)
#>     acc_tpr 
#> -0.05976471 
```
