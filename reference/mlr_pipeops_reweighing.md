# Reweighing to balance disparate impact metric

Adjusts class balance and protected group balance in order to achieve
fair(er) outcomes.

## Format

[R6::R6Class](https://r6.r-lib.org/reference/R6Class.html) object
inheriting from
[mlr3pipelines::PipeOpTaskPreproc](https://mlr3pipelines.mlr-org.com/reference/PipeOpTaskPreproc.html)/[mlr3pipelines::PipeOp](https://mlr3pipelines.mlr-org.com/reference/PipeOp.html).

## PipeOpReweighingWeights

Adds a class weight column to the
[mlr3::Task](https://mlr3.mlr-org.com/reference/Task.html) that
different
[mlr3::Learner](https://mlr3.mlr-org.com/reference/Learner.html)s may be
using. In case initial weights are present, those are multiplied with
new weights. Caution: Only fairness tasks are supported. Which means
tasks need to have protected field. `tsk$col_roles$pta`.

## PipeOpReweighingOversampling

Oversamples a [mlr3::Task](https://mlr3.mlr-org.com/reference/Task.html)
for more balanced ratios in subgroups and protected groups. Can be used
if a learner does not support weights. Caution: Only fairness tasks are
supported. Which means tasks need to have protected field.
`tsk$col_roles$pta`.

## Construction

    PipeOpReweighing*$new(id = "reweighing", param_vals = list())

- `id` (`character(1)`).

- `param_vals` ([`list()`](https://rdrr.io/r/base/list.html))

## Input and Output Channels

Input and output channels are inherited from
[mlr3pipelines::PipeOpTaskPreproc](https://mlr3pipelines.mlr-org.com/reference/PipeOpTaskPreproc.html).
Instead of a [mlr3::Task](https://mlr3.mlr-org.com/reference/Task.html),
a
[mlr3::TaskClassif](https://mlr3.mlr-org.com/reference/TaskClassif.html)
is used as input and output during training and prediction.

The output during training is the input
[mlr3::Task](https://mlr3.mlr-org.com/reference/Task.html) with added
weights column according to target class. The output during prediction
is the unchanged input.

## State

The `$state` is a named `list` with the `$state` elements inherited from
[mlr3pipelines::PipeOpTaskPreproc](https://mlr3pipelines.mlr-org.com/reference/PipeOpTaskPreproc.html).

## Parameters

- `alpha` ([`numeric()`](https://rdrr.io/r/base/numeric.html)): A number
  between 0 (no debiasing) and 1 (full debiasing).

## Internals

Introduces, or overwrites, the "weights" column in the
[mlr3::Task](https://mlr3.mlr-org.com/reference/Task.html). However, the
[mlr3::Learner](https://mlr3.mlr-org.com/reference/Learner.html) method
needs to respect weights for this to have an effect.

The newly introduced column is named `reweighing.WEIGHTS`; there will be
a naming conflict if this column already exists and is *not* a weight
column itself.

## Fields

Only fields inherited from
[mlr3pipelines::PipeOpTaskPreproc](https://mlr3pipelines.mlr-org.com/reference/PipeOpTaskPreproc.html)/[mlr3pipelines::PipeOp](https://mlr3pipelines.mlr-org.com/reference/PipeOp.html).

## Methods

Methods inherited from
[mlr3pipelines::PipeOpTaskPreproc](https://mlr3pipelines.mlr-org.com/reference/PipeOpTaskPreproc.html)/[mlr3pipelines::PipeOp](https://mlr3pipelines.mlr-org.com/reference/PipeOp.html).

## References

Kamiran, Faisal, Calders, Toon (2012). “Data preprocessing techniques
for classification without discrimination.” *Knowledge and Information
Systems*, **33**(1), 1–33.

## See also

https://mlr3book.mlr-org.com/list-pipeops.html

Other PipeOps:
[`mlr_pipeops_equalized_odds`](https://mlr3fairness.mlr-org.com/reference/mlr_pipeops_equalized_odds.md),
[`mlr_pipeops_explicit_pta`](https://mlr3fairness.mlr-org.com/reference/mlr_pipeops_explicit_pta.md)

## Super classes

[`mlr3pipelines::PipeOp`](https://mlr3pipelines.mlr-org.com/reference/PipeOp.html)
-\>
[`mlr3pipelines::PipeOpTaskPreproc`](https://mlr3pipelines.mlr-org.com/reference/PipeOpTaskPreproc.html)
-\> `PipeOpReweighingWeights`

## Methods

### Public methods

- [`PipeOpReweighingWeights$new()`](#method-PipeOpReweighingWeights-new)

- [`PipeOpReweighingWeights$clone()`](#method-PipeOpReweighingWeights-clone)

Inherited methods

- [`mlr3pipelines::PipeOp$help()`](https://mlr3pipelines.mlr-org.com/reference/PipeOp.html#method-help)
- [`mlr3pipelines::PipeOp$predict()`](https://mlr3pipelines.mlr-org.com/reference/PipeOp.html#method-predict)
- [`mlr3pipelines::PipeOp$print()`](https://mlr3pipelines.mlr-org.com/reference/PipeOp.html#method-print)
- [`mlr3pipelines::PipeOp$train()`](https://mlr3pipelines.mlr-org.com/reference/PipeOp.html#method-train)

------------------------------------------------------------------------

### Method `new()`

Creates a new instance of this
[R6::R6Class](https://mlr3pipelines.mlr-org.com/reference/PipeOpTaskPreproc.html)
R6 class.

#### Usage

    PipeOpReweighingWeights$new(id = "reweighing_wts", param_vals = list())

#### Arguments

- `id`:

  `character`  
  The PipeOps identifier in the PipeOps library.

- `param_vals`:

  `list`  
  The parameter values to be set.

  - alpha: controls the proportion between initial weight (1 if non
    existing) and reweighing weight. Defaults to 1. Here is how it
    works: new_weight = (1 - alpha) \* 1 + alpha x reweighing_weight
    final_weight = old_weight \* new_weight

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    PipeOpReweighingWeights$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Super classes

[`mlr3pipelines::PipeOp`](https://mlr3pipelines.mlr-org.com/reference/PipeOp.html)
-\>
[`mlr3pipelines::PipeOpTaskPreproc`](https://mlr3pipelines.mlr-org.com/reference/PipeOpTaskPreproc.html)
-\> `PipeOpReweighingOversampling`

## Methods

### Public methods

- [`PipeOpReweighingOversampling$new()`](#method-PipeOpReweighingOversampling-new)

- [`PipeOpReweighingOversampling$clone()`](#method-PipeOpReweighingOversampling-clone)

Inherited methods

- [`mlr3pipelines::PipeOp$help()`](https://mlr3pipelines.mlr-org.com/reference/PipeOp.html#method-help)
- [`mlr3pipelines::PipeOp$predict()`](https://mlr3pipelines.mlr-org.com/reference/PipeOp.html#method-predict)
- [`mlr3pipelines::PipeOp$print()`](https://mlr3pipelines.mlr-org.com/reference/PipeOp.html#method-print)
- [`mlr3pipelines::PipeOp$train()`](https://mlr3pipelines.mlr-org.com/reference/PipeOp.html#method-train)

------------------------------------------------------------------------

### Method `new()`

#### Usage

    PipeOpReweighingOversampling$new(id = "reweighing_os", param_vals = list())

#### Arguments

- `id`:

  \`character'  
  The PipeOp's id.

- `param_vals`:

  \`list'  
  A list of parameter values.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    PipeOpReweighingOversampling$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
library("mlr3")
library("mlr3pipelines")

reweighing = po("reweighing_wts")
learner_po = po("learner", learner = lrn("classif.rpart"))

data = tsk("adult_train")
graph = reweighing %>>% learner_po
glrn = GraphLearner$new(graph)
glrn$train(data)
tem = glrn$predict(data)
tem$confusion
#>         truth
#> response <=50K  >50K
#>    <=50K 21793  3640
#>    >50K   1275  4010
```
