to_factor = function(dataset) {
  dataset$workclass = as.factor(dataset$workclass)
  dataset$education = as.factor(dataset$education)
  dataset$martial_status = as.factor(dataset$martial_status)
  dataset$occupation = as.factor(dataset$occupation)
  dataset$relationship = as.factor(dataset$relationship)
  dataset$race = as.factor(dataset$race)
  dataset$sex = as.factor(dataset$sex)
  dataset$target = as.factor(dataset$target)
  return(dataset)
}

root = rprojroot::find_root(rprojroot::is_git_root)
adult_train = data.table::fread(file.path(root, "data-raw", "adult-train-raw.csv"))
adult_test = data.table::fread(file.path(root, "data-raw", "adult-test-raw.csv"))
adult_train = to_factor(adult_train)
adult_test = to_factor(adult_test)
usethis::use_data(adult_train, adult_test, overwrite = TRUE)
