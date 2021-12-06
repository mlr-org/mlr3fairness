load_learner_tests()

test_that("classif.fairfgrrm", {
    learner = lrn("classif.fairfgrrm")
    out = expect_learner(learner)
    simple_autotest(learner, tsk("compas")$select(cols = c("age_cat", "priors_count")))
    # result = run_autotest(learner) # FIXME: mlr3 needs to change here I think
    # expect_true(result, info = result$error)
})

test_that("regr.fairfrrm", {
    learner = lrn("regr.fairfrrm")
    out = expect_learner(learner)

    task = TaskRegr$new("long", fairml::national.longitudinal.survey, target = "income06")
    task$col_roles$pta = "gender"
    simple_autotest(learner, task)
    # result = run_autotest(learner) # FIXME: mlr3 needs to change here I think
    # expect_true(result, info = result$error)
})

test_that("regr.fairzlm", {
    learner = lrn("regr.fairzlm")
    out = expect_learner(learner)
    
    task = TaskRegr$new("long", fairml::national.longitudinal.survey, target = "income06")
    task$col_roles$pta = "gender"
    simple_autotest(learner, task)

    # result = run_autotest(learner) # FIXME: mlr3 needs to change here I think
    # expect_true(result, info = result$error)
})

test_that("classif.fairzlrm", {
    learner = lrn("classif.fairzlrm")
    out = expect_learner(learner)

    simple_autotest(learner, tsk("compas")$select(cols = c("age_cat", "priors_count")))

    # result = run_autotest(learner) # FIXME: mlr3 needs to change here I think
    # expect_true(result, info = result$error)
})

test_that("regr.fairnclm", {
    learner = lrn("regr.fairnclm")
    out = expect_learner(learner)

    task = TaskRegr$new("long", fairml::national.longitudinal.survey, target = "income06")
    task$col_roles$pta = "gender"
    simple_autotest(learner, task)

    # result = run_autotest(learner) # FIXME: mlr3 needs to change here I think
    # expect_true(result, info = result$error)
})
