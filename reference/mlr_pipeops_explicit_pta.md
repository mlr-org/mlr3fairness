# PipeOpExplicitPta

Turns the column with column role 'pta' into an explicit separate column
prefixed with "..*internal_pta*". This keeps it from getting changed or
adapted by subsequent pipelines that operate on the feature pta.

## Format

[R6::R6Class](https://r6.r-lib.org/reference/R6Class.html) object
inheriting from
[mlr3pipelines::PipeOpTaskPreproc](https://mlr3pipelines.mlr-org.com/reference/PipeOpTaskPreproc.html)/[mlr3pipelines::PipeOp](https://mlr3pipelines.mlr-org.com/reference/PipeOp.html).

## Construction

    PipeOpExplicitPta$new(id = "reweighing", param_vals = list())

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

The PipeOp does not have any hyperparameters.

## Internals

Copies the existing pta column to a new column.

## Fields

Only fields inherited from
[mlr3pipelines::PipeOpTaskPreproc](https://mlr3pipelines.mlr-org.com/reference/PipeOpTaskPreproc.html)/[mlr3pipelines::PipeOp](https://mlr3pipelines.mlr-org.com/reference/PipeOp.html).

## Methods

Methods inherited from
[mlr3pipelines::PipeOpTaskPreproc](https://mlr3pipelines.mlr-org.com/reference/PipeOpTaskPreproc.html)/[mlr3pipelines::PipeOp](https://mlr3pipelines.mlr-org.com/reference/PipeOp.html).

## See also

https://mlr3book.mlr-org.com/list-pipeops.html

Other PipeOps:
[`mlr_pipeops_EOd`](https://mlr3fairness.mlr-org.com/reference/mlr_pipeops_EOd.md),
[`mlr_pipeops_reweighing`](https://mlr3fairness.mlr-org.com/reference/mlr_pipeops_reweighing.md)

## Super classes

[`mlr3pipelines::PipeOp`](https://mlr3pipelines.mlr-org.com/reference/PipeOp.html)
-\>
[`mlr3pipelines::PipeOpTaskPreproc`](https://mlr3pipelines.mlr-org.com/reference/PipeOpTaskPreproc.html)
-\> `PipeOpExplicitPta`

## Methods

### Public methods

- [`PipeOpExplicitPta$new()`](#method-PipeOpExplicitPta-new)

- [`PipeOpExplicitPta$clone()`](#method-PipeOpExplicitPta-clone)

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

    PipeOpExplicitPta$new(id = "explicit_pta", param_vals = list())

#### Arguments

- `id`:

  `character`  
  The PipeOps identifier in the PipeOps library.

- `param_vals`:

  `list`  
  The parameter values to be set. See `Parameters`.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    PipeOpExplicitPta$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
library("mlr3")
library("mlr3pipelines")
epta = po("explicit_pta")
new = epta$train(list(tsk("adult_train")))
```
