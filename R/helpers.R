#' Score weights per group (indicated by 'pta')
#'
#' @param prediction [Prediction] A prediction
#' @param base_measure [Measure] The measure to compute in each group.
#' @param task [Task] the task prediction was made on.
#'
#' @return [numeric] Computed score
#'
#' @noRd
score_groupwise = function(prediction, base_measure, task) {
  # Get protected attribute vector
  groups = task$data(cols = task$col_roles$pta, rows = prediction$row_ids)[[1]]

  # Split prediction
  map_dbl(split(prediction$row_ids, groups), function(rws) {
    prediction$clone()$filter(rws)$score(base_measure, task = task)
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
  dt = task$data(cols = c(task$target_names, task$col_roles$pta))
  tab = as.data.table(table(dt))
  tab[, n_tgt := sum(N), by = eval(task$target_names)]
  tab[, n_pta := sum(N), by = eval(task$col_roles$pta)]
  tab[, wt := (n_tgt * n_pta) / (sum(N) * N)]
  tab[, wt := (1 - alpha) * wt + alpha * wt]

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
  prefix = "```{r read-data, include = FALSE}"
  reads = pmap_chr(list(objects, names(objects)), function(x, nm) {
    file = paste0(path, "/", nm, ".RDS")
    saveRDS(x, file = file)
    paste0(nm, " = readRDS('", basename(file), "')")
  })
  postfix = "```"
  writeLines(c(prefix, reads, postfix), paste0(path, "/read_data.Rmd"))
}