# Base Measure for Fairness

This measure extends
[`mlr3::Measure()`](https://mlr3.mlr-org.com/reference/Measure.html)
with statistical group fairness: A common approach to quantifying a
model's fairness is to compute the difference between a protected and an
unprotected group according w.r.t. some performance metric, e.g.
`classification error` (see
[`mlr3::msr()`](https://mlr3.mlr-org.com/reference/mlr_sugar.html) with
"classif.ce") or `false positive rate` (see
[`mlr3::msr()`](https://mlr3.mlr-org.com/reference/mlr_sugar.html) with
"classif.fpr"). The operation for comparison (e.g., difference or
quotient) can be specified using the `operation` parameter, e.g.
[`groupdiff_absdiff()`](https://mlr3fairness.mlr-org.com/reference/groupdiff_tau.md)
or
[`groupdiff_tau()`](https://mlr3fairness.mlr-org.com/reference/groupdiff_tau.md).

Composite measures encompasing multiple fairness metrics can be built
using
[MeasureFairnessComposite](https://mlr3fairness.mlr-org.com/reference/MeasureFairnessComposite.md).

Some popular predefined measures can be found in the
[dictionary](https://mlr3misc.mlr-org.com/reference/Dictionary.html)
[mlr3::mlr_measures](https://mlr3.mlr-org.com/reference/mlr_measures.html).

## Protected Attributes

The protected attribute is specified as a `col_role` in the
corresponding
[`mlr3::Task()`](https://mlr3.mlr-org.com/reference/Task.html):  
`<Task>$col_roles$pta = "name_of_attribute"`  
This also allows specifying more than one protected attribute, in which
case fairness will be considered on the level of intersecting groups
defined by all columns selected as a predicted attribute.

## See also

[MeasureFairnessComposite](https://mlr3fairness.mlr-org.com/reference/MeasureFairnessComposite.md)

## Super class

[`mlr3::Measure`](https://mlr3.mlr-org.com/reference/Measure.html) -\>
`MeasureFairness`

## Public fields

- `base_measure`:

  ([`mlr3::Measure()`](https://mlr3.mlr-org.com/reference/Measure.html))  
  The base measure to be used by the fairness measures, e.g.
  [`mlr3::msr()`](https://mlr3.mlr-org.com/reference/mlr_sugar.html)
  with "classif.fpr" for the false positive rate.

- `operation`:

  (`function()`)  
  The operation used to compute the difference. A function with args 'x'
  and 'y' that returns a single value. Defaults to `abs(x - y)`.

## Methods

### Public methods

- [`MeasureFairness$new()`](#method-MeasureFairness-new)

- [`MeasureFairness$clone()`](#method-MeasureFairness-clone)

Inherited methods

- [`mlr3::Measure$aggregate()`](https://mlr3.mlr-org.com/reference/Measure.html#method-aggregate)
- [`mlr3::Measure$format()`](https://mlr3.mlr-org.com/reference/Measure.html#method-format)
- [`mlr3::Measure$help()`](https://mlr3.mlr-org.com/reference/Measure.html#method-help)
- [`mlr3::Measure$obs_loss()`](https://mlr3.mlr-org.com/reference/Measure.html#method-obs_loss)
- [`mlr3::Measure$print()`](https://mlr3.mlr-org.com/reference/Measure.html#method-print)
- [`mlr3::Measure$score()`](https://mlr3.mlr-org.com/reference/Measure.html#method-score)

------------------------------------------------------------------------

### Method `new()`

Creates a new instance of this
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

#### Usage

    MeasureFairness$new(
      id = NULL,
      base_measure,
      operation = groupdiff_absdiff,
      minimize = TRUE,
      range = c(-Inf, Inf)
    )

#### Arguments

- `id`:

  (`character`)  
  The measure's id. Set to 'fairness.\<base_measure_id\>' if ommited.

- `base_measure`:

  ([`Measure()`](https://mlr3.mlr-org.com/reference/Measure.html))  
  The base metric evaluated within each subgroup.

- `operation`:

  (`function`)  
  The operation used to compute the difference. A function that returns
  a single value given input: computed metric for each subgroup.
  Defaults to
  [groupdiff_absdiff](https://mlr3fairness.mlr-org.com/reference/groupdiff_tau.md).

- `minimize`:

  ([`logical()`](https://rdrr.io/r/base/logical.html))  
  Should the measure be minimized? Defaults to `TRUE`.

- `range`:

  (`numeric(2)`)  
  Range of the resulting measure. Defaults to `c(-Inf, Inf)`.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    MeasureFairness$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
library("mlr3")
# Create MeasureFairness to measure the Predictive Parity.
t = tsk("adult_train")
learner = lrn("classif.rpart", cp = .01)
learner$train(t)
measure = msr("fairness", base_measure = msr("classif.ppv"))
predictions = learner$predict(t)
predictions$score(measure, task = t)
#> fairness.ppv 
#>    0.1202326 
```
