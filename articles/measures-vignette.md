# Fairness Metrics

## Fairness Measures

Fairness measures (or metrics) allow us to assess and audit for possible
biases in a trained model. There are several types of metrics that are
widely used in order to assess a model’s fairness. They can be coarsely
classified into three groups:

- **Statistical Group Fairness Metrics**: Given a set of predictions
  from our model, we assess for differences in one or multiple metrics
  across groups given by a *protected attribute* (Barocas, Hardt, and
  Narayanan 2019; Hardt, Price, and Srebro 2016).

- **Individual Fairness**: Basically requires that similar people are
  treated similar independent of the protected attribute (Dwork et al.
  2012). We will briefly introduce individual fairness in a dedicated
  section below.

- **Causal Fairness Notions**: An important realization in the context
  of Fairness is, that whether a process is fair is often subject to the
  underlying causal directed acyclic graph (DAG). Knowledge of the DAG
  allows for causally assessing reasons for (un-)fairness. Since DAG’s
  are often hard to construct for a given dataset, `mlr3fairness`
  currently does not provide any causal fairness metrics (Kilbertus et
  al. 2017).

### Statistical Group Fairness Metrics

One way to assess the fairness of a model is to find a definition of
fairness that is relevant to a problem at hand. We might for example
define a model to be fair if the chance to be accepted for a job given
you are qualified is independent of a protected attribute e.g. `gender`.
This can e.g. be measured using the `true positive rate`(TPR): in `mlr3`
this metric is called `"classif.tpr"`. In this case we measure
discrepancies between groups by computing differences `(-)` but we could
also compute quotients. In practice, we often compute absolute
differences.

$$\Delta_{TPR} = TPR_{Group1} - TPR_{Group2}$$

We will use metrics like the one defined above throughout the remainder
of this vignette. Some predefined measures are readily available in
`mlr3fairness`, but we will also showcase how custom measures can be
constructed below.

In general, fairness measures have a `fairness.` prefix followed by the
metric that is compared across groups. We will thus e.g. call the
difference in accuracies across groups `fairness.acc`. A full list can
be found below.

| key                  | description                                                                                               |
|:---------------------|:----------------------------------------------------------------------------------------------------------|
| fairness.acc         | Absolute differences in accuracy across groups                                                            |
| fairness.mse         | Absolute differences in mean squared error across groups                                                  |
| fairness.fnr         | Absolute differences in false negative rates across groups                                                |
| fairness.fpr         | Absolute differences in false positive rates across groups                                                |
| fairness.tnr         | Absolute differences in true negative rates across groups                                                 |
| fairness.tpr         | Absolute differences in true positive rates across groups                                                 |
| fairness.npv         | Absolute differences in negative predictive values across groups                                          |
| fairness.ppv         | Absolute differences in positive predictive values across groups                                          |
| fairness.fomr        | Absolute differences in false omission rates across groups                                                |
| fairness.fp          | Absolute differences in false positives across groups                                                     |
| fairness.tp          | Absolute differences in true positives across groups                                                      |
| fairness.tn          | Absolute differences in true negatives across groups                                                      |
| fairness.fn          | Absolute differences in false negatives across groups                                                     |
| fairness.cv          | Difference in positive class prediction, also known as Calders-Wevers gap or demographic parity           |
| fairness.eod         | Equalized Odds: Mean of absolute differences between true positive and false positive rates across groups |
| fairness.pp          | Predictive Parity: Mean of absolute differences between ppv and npv across groups                         |
| fairness.acc_eod=.05 | Accuracy under equalized odds \< 0.05 constraint                                                          |
| fairness.acc_ppv=.05 | Accuracy under ppv difference \< 0.05 constraint                                                          |

### Assessing for Bias: A first look

This vignette assumes that you are familiar with the basics of `mlr3`
and it’s core objects. The [mlr3 book](https://mlr3book.mlr-org.com/)
can be a great resource in case you want to learn more about mlr3’s
internals.

We first start by training a model for which we want to conduct an
audit. For this example, we use the `adult_train` dataset. Keep in mind
all the datasets from mlr3fairness package already set protected
attribute via the `col_role` “pta”, here the “sex” column. To speed
things up, we only use the first 1000 rows.

``` r
library(mlr3fairness)
library(mlr3learners)

t = tsk("adult_train")$filter(1:1000)
t$col_roles$pta
#> [1] "sex"
```

Our model is a random forest model fitted on the dataset:

``` r
l = lrn("classif.ranger")
l$train(t)
```

We can now predict on a new dataset and use those predictions to assess
for bias:

``` r
test = tsk("adult_test")
prd = l$predict(test)
```

Using the `$score` method and a measure we can e.g. compute the absolute
differences in true positive rates.

``` r
prd$score(msr("fairness.tpr"), task = test)
#> fairness.tpr 
#>   0.08227952
```

The exact measure to choose is often data-set and situation dependent.
The [Aequitas Fairness
Tree](https://textbook.coleridgeinitiative.org/chap-bias.html) can be a
great ressource to get started.

We can furthermore simply look at the per-group measures:

``` r
meas = groupwise_metrics(msr("classif.tpr"), test)
prd$score(meas, task = test)
#>   subgroup.tpr_Male subgroup.tpr_Female 
#>           0.8939756           0.9762551
```

### Fairness Measures - An in-depth look

Before, we have used `msr("fairness.tpr")` to assess differences in
false positive rates across groups. But what happens internally?

The [`msr()`](https://mlr3.mlr-org.com/reference/mlr_sugar.html)
function is used to obtain a `Measure` from a dictionary of pre-defined
`Measure`s. We can use
[`msr()`](https://mlr3.mlr-org.com/reference/mlr_sugar.html) without any
arguments in order to print a list of available measures. In the
following example, we will build a `Measure` that computes differences
in False Positive Rates making use of the `"classif.fpr"` measure
readily implemented in `mlr3`.

``` r
# Binary Class false positive rates
msr("classif.fpr")
#> 
#> ── <MeasureBinarySimple> (classif.fpr): False Positive Rate ────────────────────
#> • Packages: mlr3 and mlr3measures
#> • Range: [0, 1]
#> • Minimize: TRUE
#> • Average: macro
#> • Parameters: list()
#> • Properties: weights
#> • Predict type: response
#> • Predict sets: test
#> • Aggregator: mean()
```

The core `Measure` in `mlr3fairness` is a `MeasureFairness`. It can be
used to construct arbitrary measures that compute a difference between a
specific metric across groups. We can therefore build a new metric as
follows:

``` r
m1 = MeasureFairness$new(base_measure = msr("classif.fpr"), operation = function(x) {abs(x[1] - x[2])})
m1
#> 
#> ── <MeasureFairness> (fairness.fpr) ────────────────────────────────────────────
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

This measure does the following steps: - Compute the metric supplied as
`base_measure` in each group defined by the `"pta"` column. - Compute
`operation` (here `abs(x[1] - x[2])`) and return the result.

In some cases, we might also want to replace the operation with a
different operation, e.g. `x[1] / x[2]` in order to compute a different
perspective.

`mlr3fairness` comes with two built-in functions that can be used to
compute fairness metrics also across protected attributes that have more
than two classes.

- `groupdiff_absdiff`: maximum absolute difference between all classes
  (the default for all metrics)
- `groupdiff_tau`: minimum quotient between all classes

**Note**: Depending on the `operation` we need to set a different
`minimize` flag for the measure, so subsequent operations based on the
measure automatically know if the measure is to be minimized or
maximized e.g. during tuning.

We can also use those operations to construct a measure using
[`msr()`](https://mlr3.mlr-org.com/reference/mlr_sugar.html), since
`MeasureFairness` (key: `msr("fairness")`) can be constructed from the
dictionary with additional arguments.

``` r
m2 = msr("fairness", operation = groupdiff_absdiff, base_measure = msr("classif.tpr"))
```

This allows us to construct pretty flexible metrics e.g. for regression
settings:

``` r
m2 = msr("fairness", operation = groupdiff_absdiff, base_measure = msr("regr.mse"))
```

#### Non-binary protected groups

While fairness measures are widely defined or used with binary protected
attributes, we can easily extend fairness measures such that they work
with non-binary valued protected attributes.

In order to do this, we have to supply an `operation` that reduces the
desired metric measured in each subgroup to a single value. Two examples
for such operations are `groupdiff_absdiff` and `groupdiff_tau` but
custom functions can also be applied. Note, that **mlr3** `Measure`s are
designed for a scalar output and `operation` therefore always has to
result in a single scalar value.

### Composite Fairness Measures

Some fairness measures also require a combination of multiple Fairness
Metrics. In the following example we show how to compute the mean of two
fairness metrics, here false negative and true negative rates and create
a new `Measure` that computes the mean (see `aggfun`) of those metrics:

``` r
ms = list(msr("fairness.fnr"), msr("fairness.tnr"))
ms
#> [[1]]
#> 
#> ── <MeasureFairness> (fairness.fnr) ────────────────────────────────────────────
#> • Packages: mlr3 and mlr3fairness
#> • Range: [0, 1]
#> • Minimize: TRUE
#> • Average: macro
#> • Parameters: list()
#> • Properties: requires_task
#> • Predict type: response
#> • Predict sets: test
#> • Aggregator: mean()
#> 
#> [[2]]
#> 
#> ── <MeasureFairness> (fairness.tnr) ────────────────────────────────────────────
#> • Packages: mlr3 and mlr3fairness
#> • Range: [0, 1]
#> • Minimize: TRUE
#> • Average: macro
#> • Parameters: list()
#> • Properties: requires_task
#> • Predict type: response
#> • Predict sets: test
#> • Aggregator: mean()

m = MeasureFairnessComposite$new(measures = ms, aggfun = mean)
```

### How to compare the fairness performance between different learners using Benchmarks

In this example, we create a `BenchmarkInstance`. Then by using
[`aggregate()`](https://rdrr.io/r/stats/aggregate.html) function they
could access the fairness measures easily. The following example
demonstrates the process to evaluate the fairness metrics on Benchmark
Results:

``` r
design = benchmark_grid(
  tasks = tsks("adult_train"),
  learners = lrns(c("classif.ranger", "classif.rpart"),
    predict_type = "prob", predict_sets = c("train", "test")),
  resamplings = rsmps("cv", folds = 3)
)

bmr = benchmark(design)

# Operations have been set to `groupwise_quotient()`
measures = list( msr("fairness.tpr"), msr("fairness.npv"), msr("fairness.acc"), msr("classif.acc") )

tab = bmr$aggregate(measures)
tab
#>       nr     task_id     learner_id resampling_id iters fairness.tpr
#>    <int>      <char>         <char>        <char> <int>        <num>
#> 1:     1 adult_train classif.ranger            cv     3   0.05905082
#> 2:     2 adult_train  classif.rpart            cv     3   0.06003104
#>    fairness.npv fairness.acc classif.acc
#>           <num>        <num>       <num>
#> 1:   0.03588511   0.09878573   0.8634026
#> 2:   0.03479964   0.12169136   0.8405496
#> Hidden columns: resample_result
```

### Metrics aggregation - groupdiff\_\*

For `MeasureFairness`, **mlr3** computes the `base_measure` in each
group specified by the `pta` column. If we now want to return those
measures, we need to aggregate this to a single metric - e.g. using one
of the `groupdiff_*` functions available with mlr3. See
[`?groupdiff_tau`](https://mlr3fairness.mlr-org.com/reference/groupdiff_tau.md)
for a list. Note, that the `operation` below also accepts custom
aggregation function, see the example below.

``` r
msr("fairness.acc", operation = groupdiff_diff)
#> 
#> ── <MeasureFairness> (fairness.acc) ────────────────────────────────────────────
#> • Packages: mlr3 and mlr3fairness
#> • Range: [0, 1]
#> • Minimize: TRUE
#> • Average: macro
#> • Parameters: list()
#> • Properties: requires_task
#> • Predict type: response
#> • Predict sets: test
#> • Aggregator: mean()
```

We can also report other metrics, e.g. the error in a specific group:

``` r
t = tsk("adult_train")$filter(1:1000)
mm = msr("fairness.acc", operation = function(x) {x["Female"]})
l = lrn("classif.rpart")
prds = l$train(t)$predict(t)
prds$score(mm, t)
#> fairness.acc 
#>    0.9404389
```

### Individual Fairness

Individual fairness notions were first proposed by (Dwork et al. 2012).
The core idea comes from the principle of *treating similar cases
similarly and different cases differently*. In contrast to statistical
group fairness notions, this notion allows assessing *fairness* at an
individual level and would therefore allow determining whether an
individual is treated fairly. A more in-depth treatment of individual
fairness notions is given by (Binns 2020).

In order to translate this from an abstract concept into practice, we
need to define two distance metrics: - A distance metric
$d\left( x_{i},x_{j} \right)$ that measures how *similar* two cases
$x_{i}$ and $x_{j}$ are - A distance metric between treatments, here the
predictions of our model $f$:
$\phi\left( f\left( x_{i} \right),f\left( x_{j} \right) \right)$.

Intuitively, we would now want, that if $d\left( x_{i},x_{j} \right)$ is
small, the difference in predictions
$\phi\left( f\left( x_{i} \right),f\left( x_{j} \right) \right)$ should
also be small. This essentially requires Lipschitz continuity of $f$
with respect to $d$. Given a Lipschitz constant $L > 0$, we can write
this as:

$$\phi\left( f\left( x_{i} \right),f\left( x_{j} \right) \right) \leq L \cdot d\left( x_{i},x_{j} \right).$$

Currently, `mlr3fairness` does not support individual fairness metrics,
but we aim to introduce such metrics in the future.

### Using metrics for non-mlr3 predictions

We can similarly employ mlr3 metrics on predictions stemming from
different models. To do so, we create a `data.table` containing the
different components.

``` r
# Get adult data as a data.table
train = tsk("adult_train")$data()
mod = rpart::rpart(target ~ ., train)

# Predict on test data
test = tsk("adult_test")$data()
yhat = predict(mod, test, type = "vector")

# Convert to a factor with the same levels
yhat = as.factor(yhat)
levels(yhat) = levels(test$target)

compute_metrics(
  data = test, 
  target = "target",
  prediction = yhat,
  protected_attribute = "sex",
  metrics = msr("fairness.acc")
)
#> fairness.acc 
#>    0.1248581
```

Barocas, Solon, Moritz Hardt, and Arvind Narayanan. 2019. *Fairness and
Machine Learning*. fairmlbook.org.

Binns, Reuben. 2020. “On the Apparent Conflict Between Individual and
Group Fairness.” In *Proceedings of the 2020 Conference on Fairness,
Accountability, and Transparency*, 514–24. FAT\* ’20.

Dwork, Cynthia, Moritz Hardt, Toniann Pitassi, Omer Reingold, and
Richard Zemel. 2012. “Fairness Through Awareness.” In *Proceedings of
the 3rd Innovations in Theoretical Computer Science Conference*, 214–26.

Hardt, Moritz, Eric Price, and Nati Srebro. 2016. “Equality of
Opportunity in Supervised Learning.” *Advances in Neural Information
Processing Systems* 29: 3315–23.

Kilbertus, Niki, Mateo Rojas Carulla, Giambattista Parascandolo, Moritz
Hardt, Dominik Janzing, and Bernhard Schölkopf. 2017. “Avoiding
Discrimination Through Causal Reasoning.” *Advances in Neural
Information Processing Systems* 30.
