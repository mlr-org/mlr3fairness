# COMPAS Classification Task

- `compas_race_binary` : A classification task for the
  [compas](https://mlr3fairness.mlr-org.com/reference/compas.md) data
  set with the protected attribute 'race'. The observations have been
  filtered, keeping only observations with race `"Caucasian"` and
  `"African-American"`. The protected attribute has been set to
  `"race"`.

## Format

[R6::R6Class](https://r6.r-lib.org/reference/R6Class.html) inheriting
from
[mlr3::TaskClassif](https://mlr3.mlr-org.com/reference/TaskClassif.html).

## Construction

    mlr_tasks$get("compas_race_binary")
    tsk("compas_race_binary")
