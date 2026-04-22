# Classification Fair Generalized Ridge Regression Learner

Calls [fairml::fgrrm](https://rdrr.io/pkg/fairml/man/frrm.html) from
package [fairml](https://CRAN.R-project.org/package=fairml).

## Details

Fair generalized ridge regression model implemented via package
`fairml`. The 'unfairness' parameter is set to 0.05 as a default.

## Dictionary

This [mlr3::Learner](https://mlr3.mlr-org.com/reference/Learner.html)
can be instantiated via the
[dictionary](https://mlr3misc.mlr-org.com/reference/Dictionary.html)
[mlr3::mlr_learners](https://mlr3.mlr-org.com/reference/mlr_learners.html)
or with the associated sugar function
[`mlr3::lrn()`](https://mlr3.mlr-org.com/reference/mlr_sugar.html):

    mlr_learners$get("classif.fairfgrrm")
    lrn("classif.fairfgrrm")

## Meta Information

- Task type: “classif”

- Predict Types: “response”, “prob”

- Feature Types: “integer”, “numeric”, “factor”, “ordered”

- Required Packages: [mlr3](https://CRAN.R-project.org/package=mlr3),
  [fairml](https://CRAN.R-project.org/package=fairml)

## Parameters

|                |           |             |                          |                  |
|----------------|-----------|-------------|--------------------------|------------------|
| Id             | Type      | Default     | Levels                   | Range            |
| lambda         | numeric   | 0           |                          | \\\[0, \infty)\\ |
| definition     | character | sp-komiyama | sp-komiyama, eo-komiyama | \-               |
| save.auxiliary | logical   | FALSE       | TRUE, FALSE              | \-               |
| unfairness     | numeric   | \-          |                          | \\\[0, 1\]\\     |
| family         | character | binomial    | gaussian, binomial       | \-               |
| intersect      | logical   | TRUE        | TRUE, FALSE              | \-               |

## References

Scutari M, Panero F, Proissl M (2021). “Achieving Fairness with a Simple
Ridge Penalty.” *arXiv preprint arXiv:2105.13817*.

## See also

[Dictionary](https://mlr3misc.mlr-org.com/reference/Dictionary.html) of
[Learners](https://mlr3.mlr-org.com/reference/Learner.html):
[mlr3::mlr_learners](https://mlr3.mlr-org.com/reference/mlr_learners.html)

Other fairness_learners:
[`mlr_learners_classif.fairzlrm`](https://mlr3fairness.mlr-org.com/reference/mlr_learners_classif.fairzlrm.md),
[`mlr_learners_regr.fairfrrm`](https://mlr3fairness.mlr-org.com/reference/mlr_learners_regr.fairfrrm.md),
[`mlr_learners_regr.fairnclm`](https://mlr3fairness.mlr-org.com/reference/mlr_learners_regr.fairnclm.md),
[`mlr_learners_regr.fairzlm`](https://mlr3fairness.mlr-org.com/reference/mlr_learners_regr.fairzlm.md)

## Author

pfistfl

## Super classes

[`mlr3::Learner`](https://mlr3.mlr-org.com/reference/Learner.html) -\>
[`mlr3::LearnerClassif`](https://mlr3.mlr-org.com/reference/LearnerClassif.html)
-\> `LearnerClassifFairfgrrm`

## Methods

### Public methods

- [`LearnerClassifFairfgrrm$new()`](#method-LearnerClassifFairfgrrm-new)

- [`LearnerClassifFairfgrrm$clone()`](#method-LearnerClassifFairfgrrm-clone)

Inherited methods

- [`mlr3::Learner$base_learner()`](https://mlr3.mlr-org.com/reference/Learner.html#method-base_learner)
- [`mlr3::Learner$configure()`](https://mlr3.mlr-org.com/reference/Learner.html#method-configure)
- [`mlr3::Learner$encapsulate()`](https://mlr3.mlr-org.com/reference/Learner.html#method-encapsulate)
- [`mlr3::Learner$format()`](https://mlr3.mlr-org.com/reference/Learner.html#method-format)
- [`mlr3::Learner$help()`](https://mlr3.mlr-org.com/reference/Learner.html#method-help)
- [`mlr3::Learner$predict()`](https://mlr3.mlr-org.com/reference/Learner.html#method-predict)
- [`mlr3::Learner$predict_newdata()`](https://mlr3.mlr-org.com/reference/Learner.html#method-predict_newdata)
- [`mlr3::Learner$print()`](https://mlr3.mlr-org.com/reference/Learner.html#method-print)
- [`mlr3::Learner$reset()`](https://mlr3.mlr-org.com/reference/Learner.html#method-reset)
- [`mlr3::Learner$selected_features()`](https://mlr3.mlr-org.com/reference/Learner.html#method-selected_features)
- [`mlr3::Learner$train()`](https://mlr3.mlr-org.com/reference/Learner.html#method-train)

------------------------------------------------------------------------

### Method `new()`

Creates a new instance of this
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

#### Usage

    LearnerClassifFairfgrrm$new()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    LearnerClassifFairfgrrm$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
library("mlr3")
# stop example failing with warning if package not installed
learner = suppressWarnings(mlr3::lrn("classif.fairfgrrm"))
print(learner)
#> 
#> ── <LearnerClassifFairfgrrm> (classif.fairfgrrm) ───────────────────────────────
#> • Model: -
#> • Parameters: unfairness=0.05, intersect=FALSE
#> • Packages: mlr3 and fairml
#> • Predict Types: [response] and prob
#> • Feature Types: integer, numeric, factor, and ordered
#> • Encapsulation: none (fallback: -)
#> • Properties: twoclass
#> • Other settings: use_weights = 'error', predict_raw = 'FALSE'

# available parameters:
learner$param_set$ids()
#> [1] "lambda"         "definition"     "save.auxiliary" "unfairness"    
#> [5] "family"         "intersect"     
```
