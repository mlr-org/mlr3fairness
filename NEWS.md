# mlr3fairness 0.4.0

* compatibility adoption for `mlr3` 1.0.0
* replace posterdown as it is no longer available on CRAN
* Removed the 'is_recid' variable from the compas task
* Small fixes to unit tests, docs etc.

# mlr3fairness 0.3.2

* The `Task` printer now prints the protected attribute.
* `fairness_ tensor` has a new argument `normalize` that controls normalization.
  Possible values are `"all"` (global) `"group"` (by group) and `"none"` (no normalization).
*  Fix: unloading `learners`, `tasks`, `measures` and `pipeops` now removes them from dictionary.

# mlr3fairness 0.3.1

* Minor update to improve stability of unit tests and vignette building on CRAN.

# mlr3fairness 0.3.0

* CRAN release version
* Minor improvements for `groupwise_metrics`, can now `intersect` multiple protected attributes.
* New metric, `fairness.pp` that allows for computing predictive parity.
* New metric, `fairness.cv` that allows for computing the Calders-Wevers gap.
* Add `PipeOpExplicitPta` thtat copies the `pta` column into a separate column.
* Minor bug fixes and improved tests for multiple learnes and pipeops.

# mlr3fairness 0.2.0

* Added 3 types of reports: `report_modelcard`, `report_datasheet`, `report_fairness`
* Added several new fairness metrics
* Added 5 new learners (2 classification 3 regression) from package `fairml`.
  * classification: `classif.fairzlrm` | `classif.fairfgrrm`
  * regression:     `regr.fairnclm` | `regr.fairzlm` | `regr.fairfrrm`.
* Added `MeasureSubgroup` and `groupwise_metrics` that allow for inspecting dis-aggregated fairness metrics.

# mlr3fairness 0.1.0

* Added a `NEWS.md` file to track changes to the package.

