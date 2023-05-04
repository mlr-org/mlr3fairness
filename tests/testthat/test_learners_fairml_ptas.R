test_that("all learners, multi-class pta", {
    skip_on_cran()
    skip_if_not_installed("fairml")

    for (k in mlr_learners_fairness$key) {
        learner = lrn(k)
        task = test_task_multipta(task_type = learner$task_type)
        learner$train(task)
        p = learner$predict(task, row_ids = sample(task$row_ids, 5))
        expect_prediction(p)
        dt = as.data.table(p)
        expect_true(nrow(dt) == 5L)
    }
})

test_that("all learners, two pta columns", {
    skip_on_cran()
    skip_if_not_installed("fairml")
    for (k in mlr_learners_fairness$key) {
        learner = lrn(k)
        task = test_task_intersect(task_type = learner$task_type)
        learner$train(task)
        p = learner$predict(task, row_ids = sample(task$row_ids, 5))
        expect_prediction(p)
        dt = as.data.table(p)
        expect_true(nrow(dt) == 5L)
    }
})
