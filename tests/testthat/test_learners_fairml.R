test_that("classif.fairfgrrm", {
    skip_on_cran()
    skip_if_not_installed("fairml")
    learner = lrn("classif.fairfgrrm")
    out = expect_learner(learner)
    simple_autotest(learner, tsk("compas")$select(cols = c("age_cat", "priors_count")))
    result = run_autotest(learner, exclude = "sanity")
    expect_true(result, info = result$error)
})

test_that("regr.fairfrrm", {
    skip_on_cran()
    skip_if_not_installed("fairml")
    learner = lrn("regr.fairfrrm", unfairness = 0.5)
    out = expect_learner(learner)

    task = TaskRegr$new("long", fairml::national.longitudinal.survey, target = "income06")
    task$col_roles$pta = "gender"
    simple_autotest(learner, task)
    result = run_autotest(learner)
    expect_true(result, info = result$error)
})

test_that("regr.fairzlm", {
    skip_on_cran()
    skip_if_not_installed("fairml")
    learner = lrn("regr.fairzlm", unfairness = 0.5)
    out = expect_learner(learner)
    
    task = TaskRegr$new("long", fairml::national.longitudinal.survey, target = "income06")
    task$col_roles$pta = "gender"
    simple_autotest(learner, task)

    result = run_autotest(learner)
    expect_true(result, info = result$error)
})

test_that("classif.fairzlrm", {
    skip_on_cran()
    skip_if_not_installed("fairml")
    learner = lrn("classif.fairzlrm", unfairness = 0.2)
    out = expect_learner(learner)

    simple_autotest(learner, tsk("compas")$select(cols = c("age_cat", "priors_count")))

    result = run_autotest(learner, exclude = "sanity")
    expect_true(result, info = result$error)
})

test_that("regr.fairnclm", {
    skip_on_cran()
    skip_if_not_installed("fairml")
    learner = lrn("regr.fairnclm")
    out = expect_learner(learner)

    task = TaskRegr$new("long", fairml::national.longitudinal.survey, target = "income06")
    task$col_roles$pta = "gender"
    simple_autotest(learner, task)

    result = run_autotest(learner)
    expect_true(result, info = result$error)
})


