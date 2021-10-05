
# [mlr3fairness](https://github.com/mlr-org/mlr3fairness)

Machine Learning Fairness Extension for
[mlr3](https://github.com/mlr-org/mlr3).

[![tic](https://github.com/mlr-org/mlr3proba/workflows/tic/badge.svg?branch=main)](https://github.com/mlr-org/mlr3fairness/actions)
[![StackOverflow](https://img.shields.io/badge/stackoverflow-mlr3-orange.svg)](https://stackoverflow.com/questions/tagged/mlr3)
[![Mattermost](https://img.shields.io/badge/chat-mattermost-orange.svg)](https://lmmisld-lmu-stats-slds.srv.mwn.de/mlr_invite/)

# Installation

Install the development version from mlr3 repo

``` r
remotes::install_github("mlr-org/mlr3fairness")
```

# Why should I care about fairness in machine learning?

Machine Learning model predictions can be skewed by a range of factors
and thus might be considered unfair towards certain groups or
individuals. An example would be the COMPAS algorithm, which is a
popular commercial algorithm used by judges and parole officers for
scoring criminal defendant’s likelihood of reoffending (recidivism).
[Studies](https://www.propublica.org/article/machine-bias-risk-assessments-in-criminal-sentencing)
have shown, that the algorithm might be biased in favor of white
defendants. Biases can occur in a large variety of situations where
algorithms automate or support human decision making e.g. credit checks,
automatic HR tools along with a variety of other domains.

The **goal of `mlr3fairness`** is to allow for auditing of `mlr3`
learners, visualization and subsequently trying to improve fairness
using debiasing strategies.

## Feature Overview

  - **Fairness Measures:** Audit algorithmms for fairness using a
    variety of fairness criteria. This also allows for designing custom
    criteria.

  - **Fairness Visualizations:** Diagnose fairness problems through
    visualizations.

  - **Debiasing Methods:** Correct fairness problems in three lines of
    code.

  - **Fairness Report:** Obtain a report regarding an algorithm’s
    fairness. (Under development)

### Protected Attribute

`mlr3fairness` requires information about the protected attribute wrt.
which we want to assess fairness. This can be set via the `col_role`
“pta” (protected attribute). Currently `mlr3fairness` allows only a
single column as `"pta"`.

``` r
task$col_roles$pta = "variable_name"
```

In case a non-categorical or more complex protected attribute is
required, it can be manually computed and added to the task.

### Fairness Measures

`mlr3fairness` offers a variety of fairness metrics. Metrics are
prefixed with `fairness.` and can be found in the `msr()` dictionary.
Most fairness metrics are based on a difference between two protected
groups (e.g. male and female) for a given metric (e.g. the false
positive rate: `fpr`). See
[here](https://textbook.coleridgeinitiative.org/chap-bias.html) for a
more in-depth introduction to fairness metrics and how to choose them.

``` r
library(mlr3)
library(mlr3fairness)
```

| key          | description                                                                                             |
| :----------- | :------------------------------------------------------------------------------------------------------ |
| fairness.EOd | Equalized Odds: Sum of abs. difference between true positive and false positive rates across groups     |
| fairness.fpr | Abs. difference in false positive rates across groups                                                   |
| fairness.acc | Abs. difference in accurq()acy across groups (Overall accuracy equality)                                |
| fairness.tpr | Abs. difference in true positive rates across groups                                                    |
| fairness.tnr | Abs. difference in true negative rates across groups                                                    |
| fairness.ppv | Abs. difference in positive predictive values across groups (Part of Conditional use accuracy equality) |
| fairness.npv | Abs. difference in negative predictive values across groups (Part of Conditional use accuracy equality) |
| fairness.fp  | Abs. difference in false positives across groups (Part of Treatment equality)                           |
| fairness.fn  | Abs. difference in false negatives across groups (Part of Treatment equality)                           |

### Fairness Visualizations

Visualizations can be used with either a `Prediction`, `ResampleResult`
or a `BenchmarkResult`. For more information regarding those objects,
refer to the [mlr3 book](https://mlr3book.mlr-org.com/basics.html).

  - **fairness\_accuracy\_tradeoff**: Plot available trade-offs between
    fairness and model performance.

  - **compare\_metrics**: Compare fairness across models and
    cross-validation forlds.

  - **fairness\_prediction\_density**: Density plots for each protected
    attribute.

![](README_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

### Debiasing Methods

Debiasing methods can be used to improve the fairness of a given model.
`mlr3fairness` includes several methods that can be used together with
`mlr3pipelines` to obtain fair(er) models:

``` r
library(mlr3pipelines)
lrn = as_learner(po("reweighing_wts") %>>% lrn("classif.rpart"))
rs = resample(lrn, task = tsk("compas")$filter(1:500), rsmp("cv"))
rs$score(msr("fairness.acc"))
```

**Overview:**

| key             | input.type.train | input.type.predict | output.type.train | output.type.predict |
| :-------------- | :--------------- | :----------------- | :---------------- | :------------------ |
| reweighing\_os  | TaskClassif      | TaskClassif        | TaskClassif       | TaskClassif         |
| reweighing\_wts | TaskClassif      | TaskClassif        | TaskClassif       | TaskClassif         |

### Datasets

`mlr3fairness` includes two fairness datasets: `adult` and `compas`. See
`?adult` and `?compas` for additional information regarding columns.

You can load them using `tsk(<key>)`.

## Demo for Adult Dataset

We provide a short example detailing how `mlr3fairness` integrates with
the `mlr3` ecosystem.

``` r
library(mlr3fairness)

#Initialize Fairness Measure
fairness_measure = msr("fairness.fpr")
#Initialize tasks
task_train = tsk("adult_train")
task_test = tsk("adult_test")
#Initialize model
learner = lrn("classif.rpart", predict_type = "prob")

#Verify fairness metrics
learner$train(task_train)
predictions = learner$predict(task_test)
predictions$score(fairness_measure, task = task_test)

#Visualize the predicted probability score based on protected attribute.
fairness_prediction_density(predictions, task_test)
```

# Near-Future Plans

## Visualizations

2.  Improvement on visualizations, like anchor points and others. See
    issues.
3.  More visualizations.

## Metrics

1.  Add support to non-binary target attributes and non-binary protected
    attributes.

## Debiasing Methods

1.  More Debiasing Methods, post-processing and in-processing.

## Fairness Report

# Bugs, Feedbacks and Questions

mlr3fairnessWASCS is a free and open source software project that
encourages participation and feedback. If you have any issues,
questions, suggestions or feedback, please do not hesitate to open an
“issue” about it on the GitHub page\!

In case of problems / bugs, it is often helpful if you provide a
“minimum working example” that showcases the behaviour (but don’t
worry about this if the bug is obvious).
