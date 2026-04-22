# Create a Datasheet for Documenting a Dataset

Creates a new [rmarkdown](https://CRAN.R-project.org/package=rmarkdown)
template with a skeleton questionnaire for dataset documentation. Uses
the awesome markdown template created by Chris Garbin [from
Github](https://github.com/fau-masters-collected-works-cgarbin/model-card-template).

## Usage

``` r
report_datasheet(filename = "datasheet.Rmd", edit = FALSE, build = FALSE)
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

Gebru, Timnit, Morgenstern, Jamie, Vecchione, Briana, Vaughan, Wortman
J, Wallach, Hanna, III D, Hal, Crawford, Kate (2018). “Datasheets for
datasets.” *arXiv preprint arXiv:1803.09010*.

## See also

Other fairness_reports:
[`report_fairness()`](https://mlr3fairness.mlr-org.com/reference/report_fairness.md),
[`report_modelcard()`](https://mlr3fairness.mlr-org.com/reference/report_modelcard.md)

## Examples

``` r
  report_file = tempfile()
  report_datasheet(report_file)
```
