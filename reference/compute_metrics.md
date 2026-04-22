# Compute metrics for non-mlr3 predictions.

Allows computing metrics for predictions that do not stem from mlr3, and
were e.g. being made by models outside of mlr3. Currently only `classif`
and `regr` - style predictions are supported.

## Usage

``` r
compute_metrics(data, target, protected_attribute, prediction, metrics = NULL)
```

## Arguments

- data:

  (`data.table`)  
  The dataset used for predicting.

- target:

  (`character`)  
  The name of the target variable. Must be available in `data`.

- protected_attribute:

  (`character`)  
  The name(s) of the protected attributes(s). Must be available in
  `data`.

- prediction:

  (`vector`)  
  A vector containing predictions.

- metrics:

  (`Metric`\|`list`)  
  (List of) mlr3 metrics to apply.

## Protected Attributes

The protected attribute is specified as a `col_role` in the
corresponding
[`mlr3::Task()`](https://mlr3.mlr-org.com/reference/Task.html):  
`<Task>$col_roles$pta = "name_of_attribute"`  
This also allows specifying more than one protected attribute, in which
case fairness will be considered on the level of intersecting groups
defined by all columns selected as a predicted attribute.

## Examples

``` r
library("mlr3")
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
