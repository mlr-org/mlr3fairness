# Regression Non-convex Fair Regression Learner

Calls [fairml::nclm](https://rdrr.io/pkg/fairml/man/nclm.html) from
package [fairml](https://CRAN.R-project.org/package=fairml).

## Details

Fair regression model based on nonconvex optimization from Komiyama et
al. (2018). Implemented via package `fairml`. The 'unfairness' parameter
is set to 0.05 as a default.

## Dictionary

This [mlr3::Learner](https://mlr3.mlr-org.com/reference/Learner.html)
can be instantiated via the
[dictionary](https://mlr3misc.mlr-org.com/reference/Dictionary.html)
[mlr3::mlr_learners](https://mlr3.mlr-org.com/reference/mlr_learners.html)
or with the associated sugar function
[`mlr3::lrn()`](https://mlr3.mlr-org.com/reference/mlr_sugar.html):

    mlr_learners$get("regr.fairnclm")
    lrn("regr.fairnclm")

## Meta Information

- Task type: “regr”

- Predict Types: “response”

- Feature Types: “integer”, “numeric”, “factor”, “ordered”

- Required Packages: [mlr3](https://CRAN.R-project.org/package=mlr3),
  [fairml](https://CRAN.R-project.org/package=fairml)

## Parameters

|                |         |              |             |                  |
|----------------|---------|--------------|-------------|------------------|
| Id             | Type    | Default      | Levels      | Range            |
| lambda         | numeric | 0            |             | \\\[0, \infty)\\ |
| save.auxiliary | logical | FALSE        | TRUE, FALSE | \-               |
| covfun         | untyped | "stats::cov" |             | \-               |
| unfairness     | numeric | \-           |             | \\\[0, 1\]\\     |

## References

J K, A T, J H, H S (2018). “Nonconvex Optimization for Regression with
Fairness Constraints.” In *Proceedings of the 35th International
Conference on Machine Learning (ICML), PMLR 80*, 2737-2746.

## See also

[Dictionary](https://mlr3misc.mlr-org.com/reference/Dictionary.html) of
[Learners](https://mlr3.mlr-org.com/reference/Learner.html):
[mlr3::mlr_learners](https://mlr3.mlr-org.com/reference/mlr_learners.html)

Other fairness_learners:
[`mlr_learners_classif.fairfgrrm`](https://mlr3fairness.mlr-org.com/reference/mlr_learners_classif.fairfgrrm.md),
[`mlr_learners_classif.fairzlrm`](https://mlr3fairness.mlr-org.com/reference/mlr_learners_classif.fairzlrm.md),
[`mlr_learners_regr.fairfrrm`](https://mlr3fairness.mlr-org.com/reference/mlr_learners_regr.fairfrrm.md),
[`mlr_learners_regr.fairzlm`](https://mlr3fairness.mlr-org.com/reference/mlr_learners_regr.fairzlm.md)

## Author

pfistfl

## Super classes

[`mlr3::Learner`](https://mlr3.mlr-org.com/reference/Learner.html) -\>
[`mlr3::LearnerRegr`](https://mlr3.mlr-org.com/reference/LearnerRegr.html)
-\> `LearnerRegrFairnclm`

## Methods

### Public methods

- [`LearnerRegrFairnclm$new()`](#method-LearnerRegrFairnclm-new)

- [`LearnerRegrFairnclm$clone()`](#method-LearnerRegrFairnclm-clone)

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
- [`mlr3::LearnerRegr$predict_newdata_fast()`](https://mlr3.mlr-org.com/reference/LearnerRegr.html#method-predict_newdata_fast)

------------------------------------------------------------------------

### Method `new()`

Creates a new instance of this
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

#### Usage

    LearnerRegrFairnclm$new()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    LearnerRegrFairnclm$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
library("mlr3")
# stop example failing with warning if package not installed
learner = suppressWarnings(mlr3::lrn("regr.fairnclm"))
print(learner)
#> 
#> ── <LearnerRegrFairnclm> (regr.fairnclm) ───────────────────────────────────────
#> • Model: -
#> • Parameters: unfairness=0.05
#> • Packages: mlr3 and fairml
#> • Predict Types: [response]
#> • Feature Types: integer, numeric, factor, and ordered
#> • Encapsulation: none (fallback: -)
#> • Properties:
#> • Other settings: use_weights = 'error', predict_raw = 'FALSE'

# available parameters:
learner$param_set$ids()
#> [1] "lambda"         "save.auxiliary" "covfun"         "unfairness"    
```
