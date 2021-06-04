root = rprojroot::find_root(rprojroot::is_git_root)
compas = data.table::fread(file.path(root, "data-raw", "compas-scores-two-years.csv"))
usethis::use_data(compas, overwrite = TRUE)
