#' Create a Datasheet for Documenting a Dataset
#'
#' Creates a new \CRANpkg{rmarkdown} template with a skeleton questionnaire for dataset documentation.
#' Uses the awesome markdown template created by Chris Garbin
#' \href{https://github.com/fau-masters-collected-works-cgarbin/model-card-template}{from Github}.
#'
#' @param filename (`character(1)`)\cr
#'   File path or name for new file that should be created.
#' @param edit (`logical(1)`)\cr
#'   `TRUE` to edit the template immediately.
#' @param build (`logical(1)`)\cr
#'   Should the report be built after creation? Initialized to `FALSE`.
#' @references
#' `r format_bib("datasheets")`
#' @family fairness_reports
#' @export
#' @return Invisibly returns the path to the newly created file(s).
#' @examplesIf rlang::is_installed("rmarkdown")
#'   report_file = tempfile()
#'   report_datasheet(report_file)
report_datasheet = function(filename = "datasheet.Rmd", edit = FALSE, build = FALSE) {
  assert_path_for_output(filename)
  assert_flag(edit)
  assert_flag(build)
  fp = rmarkdown::draft(filename, template = "datasheets", package = "mlr3fairness", create_dir = TRUE, edit = edit)
  if (build) rmarkdown::render(fp)
  invisible(fp)
}

#' Create a Modelcard
#'
#' Creates a new \CRANpkg{rmarkdown} template with a skeleton questionnaire for a model card.
#' Uses the awesome markdown template created by Chris Garbin
#' \href{https://github.com/fau-masters-collected-works-cgarbin/model-card-template}{from Github}.
#'
#' @inheritParams report_datasheet
#' @references
#' `r format_bib("modelcards")`
#' @family fairness_reports
#' @export
#' @return Invisibly returns the path to the newly created file(s).
#' @examplesIf rlang::is_installed("rmarkdown")
#'   report_file = tempfile()
#'   report_modelcard(report_file)
report_modelcard = function(filename = "modelcard.Rmd", edit = FALSE, build = FALSE) {
  assert_path_for_output(filename)
  assert_flag(edit)
  assert_flag(build)
  fp = rmarkdown::draft(filename, template = "modelcards", package = "mlr3fairness", create_dir = TRUE, edit = edit)
  if (build) rmarkdown::render(fp)
  invisible(fp)
}

#' Create a Fairness Report
#'
#' Creates a new \CRANpkg{rmarkdown} template with a skeleton of  reported metrics and visualizations.
#' Uses the awesome markdown template created by Chris Garbin
#' \href{https://github.com/fau-masters-collected-works-cgarbin/model-card-template}{from Github}.
#'
#' @inheritParams report_datasheet
#' @param objects (`list()`)\cr
#'   A named list of objects required for the fairness report.
#'   Objects are saved as `<name>.rds` in the new folder created for the report.
#'   * `task` :: The [`mlr3::Task`] a report should be created for.
#'   * `resample_result` ::  A [mlr3::ResampleResult] result to be analyzed.
#'   * `...` :: any other objects passed on for the report.
#' @param check_objects (`logical(1)`)\cr
#'   Should items in `objects` be checked? If `FALSE`, no checks on `object` are performed.
#' @family fairness_reports
#' @export
#' @return Invisibly returns the path to the newly created file(s).
#' @examplesIf rlang::is_installed("rpart") && rlang::is_installed("rmarkdown")
#'   library("mlr3")
#'   report_file = tempfile()
#'   task = tsk("compas")
#'   learner = lrn("classif.rpart", predict_type = "prob")
#'   rr = resample(task, learner, rsmp("cv", folds = 3L))
#'   report_fairness(report_file, list(task = task, resample_result = rr))
report_fairness = function(filename = "fairness_report.Rmd", objects, edit = FALSE, check_objects = FALSE, build = FALSE) {
  assert_path_for_output(filename)
  assert_list(objects, names = "unique")
  assert_flag(edit)
  assert_flag(check_objects)
  assert_flag(build)
  if (check_objects) {
    assert_subset(c("resample_result", "task"), names(objects))
    assert_resample_result(objects$resample_result)
    assert_task(objects$task)
  }

  filepath = rmarkdown::draft(filename, template = "fairness_report", package = "mlr3fairness", create_dir = TRUE, edit = edit)
  write_files(objects, dirname(filepath))
  if (build) rmarkdown::render(filepath)
  invisible(filepath)
}
