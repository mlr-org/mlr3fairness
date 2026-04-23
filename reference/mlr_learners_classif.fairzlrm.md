# Classification Fair Logistic Regression With Covariance Constraints Learner

Calls [fairml::zlrm](https://rdrr.io/pkg/fairml/man/zlm.html) from
package [fairml](https://CRAN.R-project.org/package=fairml).

## Details

Generalized fair regression model from Zafar et al., 2019 implemented
via package `fairml`. The 'unfairness' parameter is set to 0.05 as a
default. The optimized fairness metric is statistical parity.

## Dictionary

This [mlr3::Learner](https://mlr3.mlr-org.com/reference/Learner.html)
can be instantiated via the
[dictionary](https://rdrr.io/pkg/mlr3misc/man/Dictionary.html)
[mlr3::mlr_learners](https://mlr3.mlr-org.com/reference/mlr_learners.html)
or with the associated sugar function
[`mlr3::lrn()`](https://mlr3.mlr-org.com/reference/mlr_sugar.html):

    mlr_learners$get("classif.fairzlrm")
    lrn("classif.fairzlrm")

## Meta Information

- Task type: “classif”

- Predict Types: “response”, “prob”

- Feature Types: “integer”, “numeric”, “factor”, “ordered”

- Required Packages: [mlr3](https://CRAN.R-project.org/package=mlr3),
  [fairml](https://CRAN.R-project.org/package=fairml),
  [CVXR](https://CRAN.R-project.org/package=CVXR)

## Parameters

|            |         |         |             |              |
|------------|---------|---------|-------------|--------------|
| Id         | Type    | Default | Levels      | Range        |
| unfairness | numeric | \-      |             | \\\[0, 1\]\\ |
| intersect  | logical | TRUE    | TRUE, FALSE | \-           |

## References

BJ Z, I V, M G, KP G (2019). “Fairness Constraints: a Flexible Approach
for Fair Classification.” In *Journal of Machine Learning Research, 30*,
1-42.

## See also

[Dictionary](https://rdrr.io/pkg/mlr3misc/man/Dictionary.html) of
[Learners](https://mlr3.mlr-org.com/reference/Learner.html):
[mlr3::mlr_learners](https://mlr3.mlr-org.com/reference/mlr_learners.html)

Other fairness_learners:
[`mlr_learners_classif.fairfgrrm`](https://mlr3fairness.mlr-org.com/reference/mlr_learners_classif.fairfgrrm.md),
[`mlr_learners_regr.fairfrrm`](https://mlr3fairness.mlr-org.com/reference/mlr_learners_regr.fairfrrm.md),
[`mlr_learners_regr.fairnclm`](https://mlr3fairness.mlr-org.com/reference/mlr_learners_regr.fairnclm.md),
[`mlr_learners_regr.fairzlm`](https://mlr3fairness.mlr-org.com/reference/mlr_learners_regr.fairzlm.md)

## Author

pfistfl

## Super classes

[`mlr3::Learner`](https://mlr3.mlr-org.com/reference/Learner.html) -\>
[`mlr3::LearnerClassif`](https://mlr3.mlr-org.com/reference/LearnerClassif.html)
-\> `LearnerClassifFairzlrm`

## Methods

### Public methods

- [`LearnerClassifFairzlrm$new()`](#method-LearnerClassifFairzlrm-new)

- [`LearnerClassifFairzlrm$clone()`](#method-LearnerClassifFairzlrm-clone)

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
- [`mlr3::LearnerClassif$predict_newdata_fast()`](https://mlr3.mlr-org.com/reference/LearnerClassif.html#method-predict_newdata_fast)

------------------------------------------------------------------------

### Method `new()`

Creates a new instance of this
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

#### Usage

    LearnerClassifFairzlrm$new()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    LearnerClassifFairzlrm$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
library("mlr3")
# stop example failing with warning if package not installed
learner = suppressWarnings(mlr3::lrn("classif.fairzlrm"))
print(learner)
#> 
#> ── <LearnerClassifFairzlrm> (classif.fairzlrm) ─────────────────────────────────
#> • Model: -
#> • Parameters: unfairness=0.05, intersect=FALSE
#> • Packages: mlr3, fairml, and CVXR
#> • Predict Types: [response] and prob
#> • Feature Types: integer, numeric, factor, and ordered
#> • Encapsulation: none (fallback: -)
#> • Properties: twoclass
#> • Other settings: use_weights = 'error', predict_raw = 'FALSE'

# available parameters:
learner$param_set$ids()
#> [1] "unfairness" "intersect" 
```
