#' @title Fair Learners in mlr3
#' @name mlr_learners_fairness
#'
#' @section Predefined measures:
#' \CRANpkg{mlr3fairness} comes with a set of predefined fairn learners listed below:
#'
#' `r tabular(mlr_learners_fairness)`
#'
#' @export
#' @return A data.table containing an overview of available fair learners.
#' @examples
#' # Available learners:
#' mlr_learners_fairness
mlr_learners_fairness = rowwise_table(
  ~key, ~package, ~reference,
  "regr.fairfrrm", "fairml", "Scutari et al., 2021",
  "classif.fairfgrrm", "fairml", "Scutari et al., 2021",
  "regr.fairzlm", "fairml", "Zafar et al., 2019",
  "classif.fairzlrm", "fairml", "Zafar et al., 2019",
  "regr.fairnclm", "fairml", "Komiyama et al., 2018"
)
