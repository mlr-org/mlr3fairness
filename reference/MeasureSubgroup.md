# Evaluate a metric on a subgroup

Allows for calculation of arbitrary
[`mlr3::Measure()`](https://mlr3.mlr-org.com/reference/Measure.html)s on
a selected sub-group.

## See also

[MeasureFairness](https://mlr3fairness.mlr-org.com/reference/MeasureFairness.md),
[groupwise_metrics](https://mlr3fairness.mlr-org.com/reference/groupwise_metrics.md)

## Super class

[`mlr3::Measure`](https://mlr3.mlr-org.com/reference/Measure.html) -\>
`MeasureSubgroup`

## Public fields

- `base_measure`:

  ([`mlr3::Measure()`](https://mlr3.mlr-org.com/reference/Measure.html))  
  The base measure to be used by the fairness measures, e.g.
  [`mlr3::msr()`](https://mlr3.mlr-org.com/reference/mlr_sugar.html)
  with "classif.fpr" for the false positive rate.

- `subgroup`:

  (`character`)\|(`integer`)  
  Subgroup identifier.

- `intersect`:

  (`logical`)  
  Should groups be intersected?

## Methods

### Public methods

- [`MeasureSubgroup$new()`](#method-MeasureSubgroup-new)

- [`MeasureSubgroup$clone()`](#method-MeasureSubgroup-clone)

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

    MeasureSubgroup$new(id = NULL, base_measure, subgroup, intersect = TRUE)

#### Arguments

- `id`:

  (`character`)  
  The measure's id. Set to 'fairness.\<base_measure_id\>' if ommited.

- `base_measure`:

  ([`Measure()`](https://mlr3.mlr-org.com/reference/Measure.html))  
  The measure used to measure fairness.

- `subgroup`:

  (`character`)\|(`integer`)  
  Subgroup identifier. Either value for the protected attribute or
  position in `task$levels`.

- `intersect`:

  [`logical`](https://rdrr.io/r/base/logical.html)  
  Should multiple pta groups be intersected? Defaults to `TRUE`. Only
  relevant if more than one `pta` columns are provided.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    MeasureSubgroup$clone(deep = FALSE)

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
measure = msr("subgroup", base_measure = msr("classif.acc"), subgroup = "Female")
predictions = learner$predict(t)
predictions$score(measure, task = t)
#> subgroup.acc_Female 
#>           0.9232628 
```
