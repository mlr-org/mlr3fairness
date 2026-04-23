# Task summary for fairness report

Create the general task documentation in a dataframe for fairness
report. The information includes

- Audit Date

- Task Name

- Number of observations

- Number of features

- Target Name

- Feature Names

- The Protected Attribute

## Usage

``` r
task_summary(task)
```

## Arguments

- task:

  [mlr3::Task](https://mlr3.mlr-org.com/reference/Task.html)

## Value

`data.frame` containing the reported information

## Examples

``` r
library("mlr3")
task_summary(tsk("adult_train"))
#>                                                                                                                                                              Value
#> Audit Date:                                                                                                                                             2026-04-23
#> Task Name:                                                                                                                                             adult_train
#> Number of observations:                                                                                                                                      30718
#> Number of features:                                                                                                                                             13
#> Target Name:                                                                                                                                                target
#> Feature Names:           age, capital_gain, capital_loss, education, education_num, hours_per_week, marital_status, occupation, race, relationship, sex, workclass
#> Protected Attribute(s):                                                                                                                                        sex
```
