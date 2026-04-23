# Package index

## Measuring Bias

Performance measures for fairness, based on \[Measure\].

- [`mlr_measures_fairness`](https://mlr3fairness.mlr-org.com/reference/mlr_measures_fairness.md)
  : Fairness Measures in mlr3
- [`MeasureFairness`](https://mlr3fairness.mlr-org.com/reference/MeasureFairness.md)
  : Base Measure for Fairness
- [`MeasureFairnessComposite`](https://mlr3fairness.mlr-org.com/reference/MeasureFairnessComposite.md)
  : Composite Fairness Measure
- [`MeasureFairnessConstraint`](https://mlr3fairness.mlr-org.com/reference/MeasureFairnessConstraint.md)
  : Fairness Constraint Measure
- [`MeasureSubgroup`](https://mlr3fairness.mlr-org.com/reference/MeasureSubgroup.md)
  : Evaluate a metric on a subgroup
- [`groupdiff_tau()`](https://mlr3fairness.mlr-org.com/reference/groupdiff_tau.md)
  [`groupdiff_absdiff()`](https://mlr3fairness.mlr-org.com/reference/groupdiff_tau.md)
  [`groupdiff_diff()`](https://mlr3fairness.mlr-org.com/reference/groupdiff_tau.md)
  : Groupwise Operations
- [`groupwise_metrics()`](https://mlr3fairness.mlr-org.com/reference/groupwise_metrics.md)
  : Evaluate a metric on each protected subgroup in a task.
- [`compute_metrics()`](https://mlr3fairness.mlr-org.com/reference/compute_metrics.md)
  : Compute metrics for non-mlr3 predictions.
- [`mlr_measures_positive_probability`](https://mlr3fairness.mlr-org.com/reference/mlr_measures_positive_probability.md)
  [`MeasurePositiveProbability`](https://mlr3fairness.mlr-org.com/reference/mlr_measures_positive_probability.md)
  : Positive Probability Measure

## Detecting Bias

- [`fairness_tensor()`](https://mlr3fairness.mlr-org.com/reference/fairness_tensor.md)
  : Compute the Fairness Tensor given a Prediction and a Task
- [`fairness_accuracy_tradeoff()`](https://mlr3fairness.mlr-org.com/reference/fairness_accuracy_tradeoff.md)
  : Plot Fairness Accuracy Trade-offs
- [`fairness_prediction_density()`](https://mlr3fairness.mlr-org.com/reference/fairness_prediction_density.md)
  : Probability Density Plot
- [`compare_metrics()`](https://mlr3fairness.mlr-org.com/reference/fairness_compare_metrics.md)
  : Compare different metrics

## Correcting Bias

- [`mlr_pipeops_equalized_odds`](https://mlr3fairness.mlr-org.com/reference/mlr_pipeops_equalized_odds.md)
  [`PipeOpEOd`](https://mlr3fairness.mlr-org.com/reference/mlr_pipeops_equalized_odds.md)
  : Equalized Odds Debiasing
- [`mlr_pipeops_reweighing`](https://mlr3fairness.mlr-org.com/reference/mlr_pipeops_reweighing.md)
  [`PipeOpReweighingWeights`](https://mlr3fairness.mlr-org.com/reference/mlr_pipeops_reweighing.md)
  [`PipeOpReweighingOversampling`](https://mlr3fairness.mlr-org.com/reference/mlr_pipeops_reweighing.md)
  : Reweighing to balance disparate impact metric
- [`mlr_pipeops_explicit_pta`](https://mlr3fairness.mlr-org.com/reference/mlr_pipeops_explicit_pta.md)
  [`PipeOpExplicitPta`](https://mlr3fairness.mlr-org.com/reference/mlr_pipeops_explicit_pta.md)
  : PipeOpExplicitPta

## Fair Learners

- [`mlr_learners_fairness`](https://mlr3fairness.mlr-org.com/reference/mlr_learners_fairness.md)
  : Fair Learners in mlr3

## Reports

- [`report_datasheet()`](https://mlr3fairness.mlr-org.com/reference/report_datasheet.md)
  : Create a Datasheet for Documenting a Dataset
- [`report_modelcard()`](https://mlr3fairness.mlr-org.com/reference/report_modelcard.md)
  : Create a Modelcard
- [`report_fairness()`](https://mlr3fairness.mlr-org.com/reference/report_fairness.md)
  : Create a Fairness Report

## Integrated data & tasks

- [`adult`](https://mlr3fairness.mlr-org.com/reference/adult.md)
  [`mlr_tasks_adult_test`](https://mlr3fairness.mlr-org.com/reference/adult.md)
  [`mlr_tasks_adult_train`](https://mlr3fairness.mlr-org.com/reference/adult.md)
  [`adult_test`](https://mlr3fairness.mlr-org.com/reference/adult.md)
  [`adult_train`](https://mlr3fairness.mlr-org.com/reference/adult.md) :
  Adult Dataset
- [`compas`](https://mlr3fairness.mlr-org.com/reference/compas.md)
  [`Compas`](https://mlr3fairness.mlr-org.com/reference/compas.md) :
  COMPAS Dataset
- [`mlr_tasks_compas`](https://mlr3fairness.mlr-org.com/reference/mlr_tasks_compas.md)
  : COMPAS Classification Task
- [`mlr_tasks_compas_race_binary`](https://mlr3fairness.mlr-org.com/reference/mlr_tasks_compas_race_binary.md)
  : COMPAS Classification Task
- [`task_summary()`](https://mlr3fairness.mlr-org.com/reference/task_summary.md)
  : Task summary for fairness report
