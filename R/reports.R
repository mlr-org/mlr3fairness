#' Create a datasheet for documenting a dataset
#'
#' Instantiates a new R Markdown template with a skeleton questionaire that allows for creating
#' a model card for a given ML model.
#' The interplay of code and markdown text allowed throu R Markdown
#' allows for heavily improved documentation through the use of visualizations or summary statistics.
#' Uses the awesome markdown template created by Chris Garbin
#' \href{https://github.com/fau-masters-collected-works-cgarbin/datasheet-for-dataset-template}{from Github}.
#' @param filename (`character`)\cr
#'   Filepath or name for new file that should be created.
#'   Defaults to `"datasheet.Rmd"` which creates a new file called "datasheet.Rmd" in your local working directory.
#' @param edit (`logical`)\cr
#'   `TRUE` to edit the template immediately.
#' @references
#' `r format_bib("datasheets")`
#' @examples
#' \dontrun{
#'   report_datasheet"documentation/datasheet.Rmd")
#' }
#' @export
report_datasheet = function(filename = "datasheet.Rmd", edit = FALSE) {
  assert_flag(edit)
  if (!dir.exists(dirname(filename))) {
    dir.create(dirname(filename), recursive = TRUE)
  }
  assert_path_for_output(filename)
  rmarkdown::draft(filename, template = "datasheets", package = "mlr3fairness", edit = edit)
}

#' Create a modelcard for documenting a ML model
#'
#' Instantiates a new R Markdown template with a skeleton questionaire that allows for creating
#' dataset documentation for a given dataset.
#' The interplay of code and markdown text allowed throu R Markdown
#' allows for heavily improved documentation through the use of visualizations or summary statistics.
#'
#' Uses the awesome markdown template created by Chris Garbin
#' \href{https://github.com/fau-masters-collected-works-cgarbin/model-card-template}{from Github}.
#' @param filename (`character`)\cr
#'   Filepath or name for new file that should be created.
#'   Defaults to `"modelcard.Rmd"` which creates a new file called "modelcard.Rmd" in your local working directory.
#' @param edit (`logical`)\cr
#'   `TRUE` to edit the template immediately.
#' @references
#' `r format_bib("modelcards")`
#' @examples
#' \dontrun{
#'   report_modelcard("documentation/modelcard.Rmd")
#' }
#' @export
report_modelcard = function(filename = "modelcard.Rmd", edit = FALSE) {
  assert_flag(edit)
  if (!dir.exists(dirname(filename))) {
    dir.create(dirname(filename), recursive = TRUE)
  }
  assert_path_for_output(filename)
  rmarkdown::draft(filename, template = "modelcards", package = "mlr3fairness", edit = edit)
}

#' Create a fairness report for a ML model
#'
#' Instantiates a new R Markdown template with a skeleton of reported
#' metrics and visualizations that can be used to create a fairness report.
#'
#' @param filename (`character`)\cr
#'   Filepath or name for new file that should be created.
#'   Defaults to `"modelcard.Rmd"` which creates a new file called "modelcard.Rmd" in your local working directory.
#' @param objects (`list`)\cr
#'   A named list of objects required for the fairness report.
#'   Objects are saved as `<name>.RDS` in the new folder created for the report.
#'   * `task` :: The [`Task`] a report should be created for.
#'   * `resample_result` ::  A [mlr3::ResampleResult] result to be analyzed.
#'   * `...` :: any other objects required for the report.
#' @param edit (`logical`)\cr
#'   `TRUE` to edit the template immediately.
#' @examples
#' \dontrun{
#'   task = tsk("compas")
#'   learner = lrn("classif.rpart", prob = TRUE)
#'   rr = resample(task, learner, rsmp("cv", folds = 5))
#'   report_fairness("documentation/fairness.Rmd", list(task = task, resampling_result = rr))
#' }
#' @export
report_fairness = function(filename = "fairness_report.Rmd", objects, edit = FALSE) {
  assert_list(objects)
  assert_flag(edit)
  if (!dir.exists(dirname(filename))) {
    dir.create(dirname(filename), recursive = TRUE)
  }
  assert_path_for_output(filename)
  filepath = rmarkdown::draft(filename, template = "fairness_report", package = "mlr3fairness", edit = edit)
  write_files(objects, dirname(filepath))
}



write_files = function(objects, path) {
  prefix = "```{r read-data, include = FALSE}"
  reads = pmap_chr(list(objects, names(objects)), function(x, nm) {
    file = paste0(path, "/", nm, ".RDS")
    saveRDS(x, file = file)
    paste0(nm, " = readRDS('", basename(file), "')")
  })
  postfix = "```"
  writeLines(c(prefix, reads, postfix), paste0(path, "/read_data.Rmd"))

}