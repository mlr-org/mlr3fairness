# Create a Fairness Report

Creates a new [rmarkdown](https://CRAN.R-project.org/package=rmarkdown)
template with a skeleton of reported metrics and visualizations. Uses
the awesome markdown template created by Chris Garbin [from
Github](https://github.com/fau-masters-collected-works-cgarbin/model-card-template).

## Usage

``` r
report_fairness(
  filename = "fairness_report.Rmd",
  objects,
  edit = FALSE,
  check_objects = FALSE,
  build = FALSE
)
```

## Arguments

- filename:

  (`character(1)`)  
  File path or name for new file that should be created.

- objects:

  ([`list()`](https://rdrr.io/r/base/list.html))  
  A named list of objects required for the fairness report. Objects are
  saved as `<name>.rds` in the new folder created for the report.

  - `task` :: The
    [`mlr3::Task`](https://mlr3.mlr-org.com/reference/Task.html) a
    report should be created for.

  - `resample_result` :: A
    [mlr3::ResampleResult](https://mlr3.mlr-org.com/reference/ResampleResult.html)
    result to be analyzed.

  - `...` :: any other objects passed on for the report.

- edit:

  (`logical(1)`)  
  `TRUE` to edit the template immediately.

- check_objects:

  (`logical(1)`)  
  Should items in `objects` be checked? If `FALSE`, no checks on
  `object` are performed.

- build:

  (`logical(1)`)  
  Should the report be built after creation? Initialized to `FALSE`.

## Value

Invisibly returns the path to the newly created file(s).

## See also

Other fairness_reports:
[`report_datasheet()`](https://mlr3fairness.mlr-org.com/reference/report_datasheet.md),
[`report_modelcard()`](https://mlr3fairness.mlr-org.com/reference/report_modelcard.md)

## Examples

``` r
  library("mlr3")
  report_file = tempfile()
  task = tsk("compas")
  learner = lrn("classif.rpart", predict_type = "prob")
  rr = resample(task, learner, rsmp("cv", folds = 3L))
  report_fairness(report_file, list(task = task, resample_result = rr))
```
