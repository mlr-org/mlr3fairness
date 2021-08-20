# [mlr3fairness](https://github.com/mlr-org/mlr3fairness)
## Machine Learning Fairness Extension for [mlr3](https://github.com/mlr-org/mlr3).

[![tic](https://github.com/mlr-org/mlr3proba/workflows/tic/badge.svg?branch=main)](https://github.com/mlr-org/mlr3fairness/actions)
[![StackOverflow](https://img.shields.io/badge/stackoverflow-mlr3-orange.svg)](https://stackoverflow.com/questions/tagged/mlr3)
[![Mattermost](https://img.shields.io/badge/chat-mattermost-orange.svg)](https://lmmisld-lmu-stats-slds.srv.mwn.de/mlr_invite/)

# Installation

Install the development version from mlr3 repo

```r
remotes::install_github("mlr-org/mlr3fairness")
```

# Why should you care fairness in machine learning?

Because fairness is important in machine learning. ML model predictions could be skewed by a range of factors and thus might be considered unfair to certain groups or individuals. An example would be the COMPAS dataset, the african american community has higher FP than FN. This will lead to biased predictions and even more, higher prison rates for african american than the other races.


# What is mlr3fairness?

mlr3fairness is a comprehensive machine learning package with full support from mlr3. There are three major components of this package:
* **Fairness Measures:** Provide fairness metrics (You could also create your own!!!)
* **Fairness Visualizations:** Diagnose fairness problems through visualizations with one function call.
* **Debiasing Methods:**Correct fairness problems in three lines of code.

# Feature Overview

* Fairness Measures

**Fairness metrics are the foundations.**

Not only you can only use those metrics to diagnose biases in models. You also need those measures to build fairness visualizations or bias mitigation algorithms in order to detect the fairness problems and correct them.

* Fairness Visualizations

**Fairness Visualizations are the communications.**

You are not expected to understand fairness problems from just the fairness metrics. They are just numbers for non-experts. However, through fairness visualizations we could provide a clear, thorough and understandable way to understand the fairness problems.

* Debiasing Methods

**Debiasing Algorithms are the Solutions.**

Now you could measure, see and understand the fairness problems. You want to mitigate the fairness problems that exist in their models. For this part, mlr3fairness will provide you the toolkits like reweighing or equalized odds algorithms in `FairnessPipeOps`. Your models could now correct fairness problems with one extra line of code.

# Demo for Adult Dataset

# Near-Future Plans

## Visualizations
1. Should we add support to BenchmarkResult and Resampling result to fairness_prediction_density visulization, and how does it look like.
2. Improvement on visualizations, like anchor points and others. See issues.
3. More visualizations.

## Metrics
1. Add support to non-binary target attributes and non-binary protected attributes.
2. Add metrics that are not supported with current design in `extra_measures.R`.

## Debiasing Methods
1. More Debiasing Methods, post-processing and in-processing.

# Bugs, Feedbacks and Questions

mlr3fairnessWASCS is a free and open source software project that encourages participation and feedback. If you have any issues, questions, suggestions or feedback, please do not hesitate to open an “issue” about it on the GitHub page!

In case of problems / bugs, it is often helpful if you provide a “minimum working example” that showcases the behaviour (but don’t worry about this if the bug is obvious).

# GSOC 2021 Project Candidate

This project has been accepted as a Google Summer of Code Project!

* Project description: https://github.com/rstats-gsoc/gsoc2021/wiki/mlr3fairness
* Timeline: https://developers.google.com/open-source/gsoc/timeline
* Student: @superp0tat0
* Mentors: @pfistfl, @mllg, @berndbischl, @vollmersj

