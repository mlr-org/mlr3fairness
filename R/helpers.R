#' Score weights per group (indicated by 'pta')
#'
#' @param prediction [Prediction] A prediction
#' @param base_measure [Measure] The measure to compute in each group.
#' @param task [Task] the task prediction was made on.
#' @param ... `any` passed on to respective measure.
#' @return [numeric] Computed score
#'
#' @noRd
score_groupwise = function(prediction, base_measure, task, ...) {
  # Get protected attribute vector
  groups = get_pta(task, rows = prediction$row_ids)

  # Split prediction
  map_dbl(split(prediction$row_ids, groups), function(rws) {
    prediction$clone()$filter(rws)$score(base_measure, task = task, ...)
  })
}

#' Compute weights for PipeOpReweighing*
#'
#' @param task [Task] the task
#' @param alpha [numeric] Debiasing strength
#'
#' @return [data.table] A data.table with counts and weights for each feature.
#' @noRd
compute_reweighing_weights = function(task, alpha = 1) {
  pta = task$col_roles$pta
  dt = task$data(cols = c(task$target_names, pta))
  tab = as.data.table(table(dt))
  tab[, n_tgt := sum(N), by = eval(task$target_names)]
  tab[, n_pta := sum(N), by = eval(pta)]
  tab[, wt := (n_tgt * n_pta) / (sum(N) * N)]
  tab[, wt := (1 - alpha) * wt + alpha * wt]

  # Ensure correct feature type
  if (all(map_lgl(pta, function(x) {inherits(dt[[x]], "integer")}))) {
    set(tab, j = task$col_roles$pta, value = as.integer(tab[[pta]]))
  } else if (all(map_lgl(pta, function(x) {inherits(dt[[x]], "numeric")}))) {
    set(tab, j = task$col_roles$pta, value = as.numeric(tab[[pta]]))
  }

  return(tab)
}

#' Same as task$filter(), but allows duplicate row IDs
#'
#' @param task [Task] the task
#' @param row_ids [numeric] the row IDs to select
#'
#' @return [Task] the modified task
#' @noRd
task_filter_ex = function(task, row_ids) {
  added_rows = row_ids[duplicated(row_ids)]
  new_rows = task$nrow + seq_along(added_rows)
  if (length(added_rows)) {
    task$rbind(task$data(rows = added_rows))
  }

  # row ids can be anything, we just take what mlr3 happens to assign.
  row_ids[duplicated(row_ids)] = task$row_ids[new_rows]
  task$filter(row_ids)
}

#' Write objects as .RDS files into path.
#'
#' @param objects [list] list of objects
#' @param path [character] path to save to
#'
#' @return NULL
#' @noRd
write_files = function(objects, path) {
  reads = pmap_chr(list(objects, names(objects)), function(x, nm) {
    fn = sprintf("%s.rds", nm)
    saveRDS(x, file = file.path(path, fn))
    paste0(nm, " = readRDS('", fn, "')")
  })

  writeLines(con = paste0(path, "/read_data.Rmd"), c(
    "```{r read-data, include = FALSE}",
    reads,
    "```"))
}

replace_prefix = function(str, prefix, replacement) {
  assert_character(prefix, any.missing = FALSE)
  assert_string(replacement)
  pattern = sprintf("^(%s)\\.", paste0(prefix, collapse = "|"))
  sub(pattern, replacement, str)
}

tabular = function(df, ...) {
  stopifnot(is.data.frame(df))

  align = function(x) if (is.numeric(x)) "r" else "l"
  col_align = map_chr(df, align)

  cols = lapply(df, format, ...)
  contents = do.call("paste",
    c(cols, list(sep = " \\tab ", collapse = "\\cr\n   ")))

  paste("\\tabular{", paste(col_align, collapse = ""), "}{\n   ",
    paste0("\\strong{", names(df), "}", sep = "", collapse = " \\tab "), " \\cr\n   ",
    contents, "\n }\n", sep = "")
}

pprob_to_matrix <- function(pp, task) {
  y <- matrix(c(pp, 1 - pp), ncol = 2L, nrow = length(pp))
  colnames(y) <- task$class_names
  y
}

int_to_numeric = function(p) {
  ints = colnames(keep(p, is.integer))
  if (length(ints)) p = p[, (ints) := map(.SD, as.numeric), .SDcols = ints]
  return(p)
}

#' Score weights per group (indicated by 'pta')
#'
#' @param task [Task]
#' @param rows (`integer`) rows to get
#' @param intersect (`logical`) should groups be intersected? 
#'   If `TRUE` all pta columns are combined into a single column factor.
#'   If `FALSE`, returns all pta columns converted to factors.
#' @return data.table vector of group assignments.
#' @noRd
get_pta = function(task, rows = NULL, intersect = FALSE) {
  assert_flag(intersect)
  groups = task$data(cols = task$col_roles$pta, rows = rows)
  if (ncol(groups) >= 2L && intersect) {
    groups = groups[, "newpta" := apply(.SD, 1, str_collapse)][, "newpta"]
  } else {
    groups = groups[, (task$col_roles$pta) := map(.SD, as.factor), .SDcols = task$col_roles$pta]
  }
}

#' @title Task summary for fairness report
#'
#' @description 
#' Create the general task documentation in a dataframe for fairness report.
#' The information includes
#' * Audit Date
#' * Task Name
#' * Number of observations
#' * Number of features
#' * Target Name
#' * Feature Names
#' * The Protected Attribute
#'
#' @param task [Task]
#' @return 
#' `data.frame` containing the reported information
#' @examples
#' library("mlr3")
#' task_summary(tsk("adult_train"))
#' @export
task_summary = function(task) {
  # Create the report attributes
  report_attr = list(
    "Audit Date: ",
    "Task Name: ",
    "Number of observations: ",
    "Number of features: ",
    "Target Name: ",
    "Feature Names: ",
    "Protected Attribute(s): "
  )

  # Create the skeleton of summary table
  summary_table <- data.frame(Value=rep(NA, length(report_attr)))
  rownames(summary_table) <- report_attr

  #Assign the value to the table
  summary_table$Value = c(
    as.character(Sys.Date()),
    task$id,
    task$nrow,
    task$ncol,
    task$target_names,
    paste(task$feature_names, collapse = ", "),
    task$col_roles$pta
  )

  return(summary_table)
}
