# Create a Modelcard

Creates a new [rmarkdown](https://CRAN.R-project.org/package=rmarkdown)
template with a skeleton questionnaire for a model card. Uses the
awesome markdown template created by Chris Garbin [from
Github](https://github.com/fau-masters-collected-works-cgarbin/model-card-template).

## Usage

``` r
report_modelcard(filename = "modelcard.Rmd", edit = FALSE, build = FALSE)
```

## Arguments

- filename:

  (`character(1)`)  
  File path or name for new file that should be created.

- edit:

  (`logical(1)`)  
  `TRUE` to edit the template immediately.

- build:

  (`logical(1)`)  
  Should the report be built after creation? Initialized to `FALSE`.

## Value

Invisibly returns the path to the newly created file(s).

## References

Mitchell, Margaret, Wu, Simone, Zaldivar, Andrew, Barnes, Parker,
Vasserman, Lucy, Hutchinson, Ben, Spitzer, Elena, Raji, Deborah I,
Gebru, Timnit (2019). “Model cards for model reporting.” In *Proceedings
of the conference on fairness, accountability, and transparency*,
220–229.

## See also

Other fairness_reports:
[`report_datasheet()`](https://mlr3fairness.mlr-org.com/reference/report_datasheet.md),
[`report_fairness()`](https://mlr3fairness.mlr-org.com/reference/report_fairness.md)

## Examples

``` r
  report_file = tempfile()
  report_modelcard(report_file)
```
