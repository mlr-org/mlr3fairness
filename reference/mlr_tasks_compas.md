# COMPAS Classification Task

Derived tasks:  

- `compas` : A classification task for the
  [compas](https://mlr3fairness.mlr-org.com/reference/compas.md) data
  set with the protected attribute 'sex'.

## Format

[R6::R6Class](https://r6.r-lib.org/reference/R6Class.html) inheriting
from
[mlr3::TaskClassif](https://mlr3.mlr-org.com/reference/TaskClassif.html).

## Construction

    mlr_tasks$get("compas")
    tsk("compas")
