test_that("classif.fairfgrrm", {
    skip_if_not_installed("fairml")
    learner = lrn("classif.fairfgrrm")
    out = expect_learner(learner)
    simple_autotest(learner, tsk("compas")$select(cols = c("age_cat", "priors_count")))
    # result = run_autotest(learner) # FIXME: mlr3 needs to change here I think
    # expect_true(result, info = result$error)
})

test_that("regr.fairfrrm", {
    skip_if_not_installed("fairml")
    learner = lrn("regr.fairfrrm")
    out = expect_learner(learner)

    task = TaskRegr$new("long", fairml::national.longitudinal.survey, target = "income06")
    task$col_roles$pta = "gender"
    simple_autotest(learner, task)
    # result = run_autotest(learner) # FIXME: mlr3 needs to change here I think
    # expect_true(result, info = result$error)
})

test_that("regr.fairzlm", {
    skip_if_not_installed("fairml")
    learner = lrn("regr.fairzlm")
    out = expect_learner(learner)
    
    task = TaskRegr$new("long", fairml::national.longitudinal.survey, target = "income06")
    task$col_roles$pta = "gender"
    simple_autotest(learner, task)

    # result = run_autotest(learner) # FIXME: mlr3 needs to change here I think
    # expect_true(result, info = result$error)
})

test_that("classif.fairzlrm", {
    skip_if_not_installed("fairml")
    learner = lrn("classif.fairzlrm")
    out = expect_learner(learner)

    simple_autotest(learner, tsk("compas")$select(cols = c("age_cat", "priors_count")))

    # result = run_autotest(learner) # FIXME: mlr3 needs to change here I think
    # expect_true(result, info = result$error)
})

test_that("regr.fairnclm", {
    skip_if_not_installed("fairml")
    learner = lrn("regr.fairnclm")
    out = expect_learner(learner)

    task = TaskRegr$new("long", fairml::national.longitudinal.survey, target = "income06")
    task$col_roles$pta = "gender"
    simple_autotest(learner, task)

    # result = run_autotest(learner) # FIXME: mlr3 needs to change here I think
    # expect_true(result, info = result$error)
})


