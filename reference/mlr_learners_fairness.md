# Fair Learners in mlr3

Fair Learners in mlr3

## Usage

``` r
mlr_learners_fairness
```

## Format

An object of class `data.table` (inherits from `data.frame`) with 5 rows
and 3 columns.

## Value

A data.table containing an overview of available fair learners.

## Predefined measures

[mlr3fairness](https://CRAN.R-project.org/package=mlr3fairness) comes
with a set of predefined 'fair learners' listed below:

|                   |             |                       |
|-------------------|-------------|-----------------------|
| **key**           | **package** | **reference**         |
| regr.fairfrrm     | fairml      | Scutari et al., 2021  |
| classif.fairfgrrm | fairml      | Scutari et al., 2021  |
| regr.fairzlm      | fairml      | Zafar et al., 2019    |
| classif.fairzlrm  | fairml      | Zafar et al., 2019    |
| regr.fairnclm     | fairml      | Komiyama et al., 2018 |

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
# example code

library("mlr3")
# Available learners:
mlr_learners_fairness
#>                  key package             reference
#>               <char>  <char>                <char>
#> 1:     regr.fairfrrm  fairml  Scutari et al., 2021
#> 2: classif.fairfgrrm  fairml  Scutari et al., 2021
#> 3:      regr.fairzlm  fairml    Zafar et al., 2019
#> 4:  classif.fairzlrm  fairml    Zafar et al., 2019
#> 5:     regr.fairnclm  fairml Komiyama et al., 2018
```
