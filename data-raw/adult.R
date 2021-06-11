root = rprojroot::find_root(rprojroot::is_git_root)
adult_train = data.table::fread(file.path(root, "data-raw", "adult-test-raw.csv"))
adult_test = data.table::fread(file.path(root, "data-raw", "adult-train-raw.csv"))
usethis::use_data(adult_train, adult_test, overwrite = TRUE)
