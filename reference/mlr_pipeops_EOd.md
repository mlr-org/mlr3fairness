# Equalized Odds Debiasing

Fairness post-processing method to achieve equalized odds fairness.
Works by randomly flipping a subset of predictions with pre-computed
probabilities in order to satisfy equalized odds constraints.  
NOTE: Carefully assess the correct privileged group.

## Format

[R6::R6Class](https://r6.r-lib.org/reference/R6Class.html) object
inheriting from
[mlr3pipelines::PipeOpTaskPreproc](https://mlr3pipelines.mlr-org.com/reference/PipeOpTaskPreproc.html)/
[mlr3pipelines::PipeOp](https://mlr3pipelines.mlr-org.com/reference/PipeOp.html).

## Construction

    PipeOpEOd*$new(id = "eod", param_vals = list())

- `id` (`character(1))`.

- `param_vals` ([`list()`](https://rdrr.io/r/base/list.html))

## Input and Output Channels

Input and output channels are inherited from
[mlr3pipelines::PipeOpTaskPreproc](https://mlr3pipelines.mlr-org.com/reference/PipeOpTaskPreproc.html).
Instead of a [mlr3::Task](https://mlr3.mlr-org.com/reference/Task.html),
a
[mlr3::TaskClassif](https://mlr3.mlr-org.com/reference/TaskClassif.html)
is used as input and output during training and prediction.

The output during training is the input
[mlr3::Task](https://mlr3.mlr-org.com/reference/Task.html). The output
during prediction is a
[mlr3::PredictionClassif](https://mlr3.mlr-org.com/reference/PredictionClassif.html)
with partially flipped predictions.

## State

The `$state` is a named list with the `$state` elements inherited from
[mlr3pipelines::PipeOpTaskPreproc](https://mlr3pipelines.mlr-org.com/reference/PipeOpTaskPreproc.html).

## Parameters

- `alpha` (numeric): A number between 0 (no debiasing) and 1 (full
  debiasing). Controls the debiasing strength by multiplying the
  flipping probabilities with alpha.

- `privileged` (character): The privileged group.

## Fields

Only fields inherited from
[mlr3pipelines::PipeOpTaskPreproc](https://mlr3pipelines.mlr-org.com/reference/PipeOpTaskPreproc.html)/[mlr3pipelines::PipeOp](https://mlr3pipelines.mlr-org.com/reference/PipeOp.html).

## Methods

Methods inherited from
[mlr3pipelines::PipeOpTaskPreproc](https://mlr3pipelines.mlr-org.com/reference/PipeOpTaskPreproc.html)/[mlr3pipelines::PipeOp](https://mlr3pipelines.mlr-org.com/reference/PipeOp.html).

## References

Hardt M, Price E, Srebro N (2016). “Equality of Opportunity in
Supervised Learning.” In *Advances in Neural Information Processing
Systems*, volume 29, 3315–3323.
<https://papers.nips.cc/paper/2016/file/9d2682367c3935defcb1f9e247a97c0d-Paper.pdf>.

Pleiss, Geoff, Raghavan, Manish, Wu, Felix, Kleinberg, Jon, Weinberger,
Q K (2017). “On Fairness and Calibration.” In Guyon I, Luxburg UV,
Bengio S, Wallach H, Fergus R, Vishwanathan S, Garnett R (eds.),
*Advances in Neural Information Processing Systems*, volume 30.
<https://proceedings.neurips.cc/paper/2017/file/b8b9c74ac526fffbeb2d39ab038d1cd7-Paper.pdf>.

## See also

https://mlr3book.mlr-org.com/list-pipeops.html

Other PipeOps:
[`mlr_pipeops_explicit_pta`](https://mlr3fairness.mlr-org.com/reference/mlr_pipeops_explicit_pta.md),
[`mlr_pipeops_reweighing`](https://mlr3fairness.mlr-org.com/reference/mlr_pipeops_reweighing.md)

## Super class

[`mlr3pipelines::PipeOp`](https://mlr3pipelines.mlr-org.com/reference/PipeOp.html)
-\> `PipeOpEOd`

## Methods

### Public methods

- [`PipeOpEOd$new()`](#method-PipeOpEOd-new)

- [`PipeOpEOd$clone()`](#method-PipeOpEOd-clone)

Inherited methods

- [`mlr3pipelines::PipeOp$help()`](https://mlr3pipelines.mlr-org.com/reference/PipeOp.html#method-help)
- [`mlr3pipelines::PipeOp$predict()`](https://mlr3pipelines.mlr-org.com/reference/PipeOp.html#method-predict)
- [`mlr3pipelines::PipeOp$print()`](https://mlr3pipelines.mlr-org.com/reference/PipeOp.html#method-print)
- [`mlr3pipelines::PipeOp$train()`](https://mlr3pipelines.mlr-org.com/reference/PipeOp.html#method-train)

------------------------------------------------------------------------

### Method `new()`

Creates a new instance of this
[R6::R6Class](https://mlr3pipelines.mlr-org.com/reference/PipeOp.html)
R6 class.

#### Usage

    PipeOpEOd$new(id = "EOd", param_vals = list())

#### Arguments

- `id`:

  character  
  The PipeOps identifier in the PipeOps library.

- `param_vals`:

  list  
  The parameter values to be set. See `Parameters`.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    PipeOpEOd$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
library("mlr3")
library("mlr3pipelines")

eod = po("EOd")
learner_po = po("learner_cv",
  learner = lrn("classif.rpart"),
  resampling.method = "insample"
)

task = tsk("compas")
graph = learner_po %>>% eod
glrn = GraphLearner$new(graph)
glrn$train(task)

# On a Task
glrn$predict(task)
#> 
#> ── <PredictionClassif> for 6172 observations: ──────────────────────────────────
#>  row_ids truth response
#>        1     0        0
#>        2     1        0
#>        3     1        1
#>      ---   ---      ---
#>     6170     0        0
#>     6171     0        0
#>     6172     1        1

# On newdata
glrn$predict_newdata(task$data(cols = task$feature_names))
#> 
#> ── <PredictionClassif> for 6172 observations: ──────────────────────────────────
#>  row_ids truth response
#>        1  <NA>        0
#>        2  <NA>        0
#>        3  <NA>        0
#>      ---   ---      ---
#>     6170  <NA>        0
#>     6171  <NA>        0
#>     6172  <NA>        1
```
