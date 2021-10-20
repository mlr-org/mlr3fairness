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
#'   use_datasheet"documentation/datasheet.Rmd")
#' }
#' @export
use_datasheet = function(filename = "datasheet.Rmd", edit = TRUE) {
  assert_path_for_output(filename)
  assert_flag(edit)
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
#'   use_modelcard("documentation/modelcard.Rmd")
#' }
#' @export
use_modelcard = function(filename = "modelcard.Rmd", edit = TRUE) {
  assert_path_for_output(filename)
  assert_flag(edit)
  rmarkdown::draft(filename, template = "modelcards", package = "mlr3fairness", edit = edit)
}