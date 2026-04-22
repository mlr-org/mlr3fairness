# Composite Fairness Measure

Computes a composite measure from multiple fairness metrics and
aggregates them using `aggfun` (defaulting to
[`mean()`](https://rdrr.io/r/base/mean.html)).

## Protected Attributes

The protected attribute is specified as a `col_role` in the
corresponding
[`mlr3::Task()`](https://mlr3.mlr-org.com/reference/Task.html):  
`<Task>$col_roles$pta = "name_of_attribute"`  
This also allows specifying more than one protected attribute, in which
case fairness will be considered on the level of intersecting groups
defined by all columns selected as a predicted attribute.

## Super class

[`mlr3::Measure`](https://mlr3.mlr-org.com/reference/Measure.html) -\>
`MeasureFairnessComposite`

## Methods

### Public methods

- [`MeasureFairnessComposite$new()`](#method-MeasureFairnessComposite-new)

- [`MeasureFairnessComposite$clone()`](#method-MeasureFairnessComposite-clone)

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

    MeasureFairnessComposite$new(
      id = NULL,
      measures,
      aggfun = function(x) mean(x),
      operation = groupdiff_absdiff,
      minimize = TRUE,
      range = c(-Inf, Inf)
    )

#### Arguments

- `id`:

  (`character(1)`)  
  Id of the measure. Defaults to the concatenation of ids in `measure`.

- `measures`:

  (list of
  [MeasureFairness](https://mlr3fairness.mlr-org.com/reference/MeasureFairness.md))  
  List of fairness measures to aggregate.

- `aggfun`:

  (`function()`)  
  Aggregation function used to aggregate results from respective
  measures. Defaults to `sum`.

- `operation`:

  (`function()`)  
  The operation used to compute the difference. A function that returns
  a single value given input: computed metric for each subgroup.
  Defaults to `groupdiff_absdiff`. See `MeasureFairness` for more
  information.

- `minimize`:

  (`logical(1)`)  
  Should the measure be minimized? Defaults to `TRUE`.

- `range`:

  (`numeric(2)`)  
  Range of the resulting measure. Defaults to `c(-Inf, Inf)`.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    MeasureFairnessComposite$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
library("mlr3")
# Equalized Odds Metric
MeasureFairnessComposite$new(measures = msrs(c("fairness.fpr", "fairness.tpr")))
#> 
#> ── <MeasureFairnessComposite> (fairness.fpr_tpr) ───────────────────────────────
#> • Packages: mlr3 and mlr3fairness
#> • Range: [-Inf, Inf]
#> • Minimize: TRUE
#> • Average: macro
#> • Parameters: list()
#> • Properties: requires_task
#> • Predict type: response
#> • Predict sets: test
#> • Aggregator: mean()

# Other metrics e.g. based on negative rates
MeasureFairnessComposite$new(measures = msrs(c("fairness.fnr", "fairness.tnr")))
#> 
#> ── <MeasureFairnessComposite> (fairness.fnr_tnr) ───────────────────────────────
#> • Packages: mlr3 and mlr3fairness
#> • Range: [-Inf, Inf]
#> • Minimize: TRUE
#> • Average: macro
#> • Parameters: list()
#> • Properties: requires_task
#> • Predict type: response
#> • Predict sets: test
#> • Aggregator: mean()
```
