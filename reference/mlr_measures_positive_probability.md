# Positive Probability Measure

Return the probabiliy of a positive prediction, often known as
'Calders-Wevers' gap. This is defined as count of positive predictions
divided by the number of observations.

## Super class

[`mlr3::Measure`](https://mlr3.mlr-org.com/reference/Measure.html) -\>
`MeasurePositiveProbability`

## Methods

### Public methods

- [`MeasurePositiveProbability$new()`](#method-MeasurePositiveProbability-new)

- [`MeasurePositiveProbability$clone()`](#method-MeasurePositiveProbability-clone)

Inherited methods

- [`mlr3::Measure$aggregate()`](https://mlr3.mlr-org.com/reference/Measure.html#method-aggregate)
- [`mlr3::Measure$format()`](https://mlr3.mlr-org.com/reference/Measure.html#method-format)
- [`mlr3::Measure$help()`](https://mlr3.mlr-org.com/reference/Measure.html#method-help)
- [`mlr3::Measure$print()`](https://mlr3.mlr-org.com/reference/Measure.html#method-print)
- [`mlr3::Measure$score()`](https://mlr3.mlr-org.com/reference/Measure.html#method-score)

------------------------------------------------------------------------

### Method `new()`

Initialize a Measure Positive Probability Object

#### Usage

    MeasurePositiveProbability$new()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    MeasurePositiveProbability$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
library("mlr3")
# Create Positive Probability Measure
t = tsk("adult_train")
learner = lrn("classif.rpart", cp = .01)
learner$train(t)
measure = msr("classif.pp")
predictions = learner$predict(t)
predictions$score(measure, task = t)
#> classif.pp 
#>  0.8342014 
```
