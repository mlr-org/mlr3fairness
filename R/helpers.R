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

#' Score weights per group (indicated by 'pta')
#'
#' @param task [Task]
#' @param rows (`integer`) rows to get
#' @param intersectional (`logical`) should groups be intersected? If `FALSE` only first pta is used.
#' @return character vector of group assignments
#'
#' @noRd
get_pta = function(task, rows, intersectional = TRUE) {
  assert_flag(intersectional)
  groups = task$data(cols = task$col_roles$pta, rows = rows)
  if (ncol(groups) >= 2L && intersectional) {
    factor(pmap_chr(groups, paste, sep = "_"))
  } else {
    factor(groups[[1]])
  }
}