# Reports

`mlr3fairness` contains several templates that allow for creating
reports based on `RMarkdown` files. The `report_*` functions instantiate
a new `.Rmd` file that can be further adapted by the user.

The following reports are currently available in `mlr3fairness`.

| Report             | Description             | Reference             |
|--------------------|-------------------------|-----------------------|
| `report_modelcard` | Modelcard for ML models | Mitchell et al., 2018 |
| `report_datasheet` | Datasheet for data sets | Gebru et al., 2018    |
| `report_fairness`  | Fairness Report         | –                     |

**Usage:**

Templates contain a set of pre-defined questions which can be used for
reporting as well as initial graphics. The created `.Rmd` file can then
be extended by the user. It can later be converted into a `html` report
using
[`rmarkdown::render()`](https://pkgs.rstudio.com/rmarkdown/reference/render.html).

``` r
library(mlr3fairness)
rmdfile = report_datasheet()
rmarkdown::render(rmdfile)
```

## Examples

#### [Example: Model Card](https://mlr3fairness.mlr-org.com/articles/modelcard/modelcard.html)

#### [Example: Data Sheet](https://mlr3fairness.mlr-org.com/articles/datasheet/datasheet.html)

#### [Example: Fairness Report](https://mlr3fairness.mlr-org.com/articles/fairness/fairness.html)
