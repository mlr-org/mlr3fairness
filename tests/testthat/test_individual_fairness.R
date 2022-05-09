test_that("classif - multiclass", {
    rid = c(4,7, 9, 10:100)
    t = tsk("iris")$filter(rid)
    l = lrn("classif.rpart")
    l$train(t)
    prd = l$predict(t)

    ifs = individual_fairness(t$data(), prd)

    # Compare to manual computation
    x = gower::gower_dist(t$data()[1,], t$data())[-1]
    y = gower::gower_dist(data.table(prd$data$response), data.table(prd$data$response[1]))[-1]
    expect_equal(mean(abs(y) - x),  ifs[1]$individual_fairness)

    expect_set_equal(ifs$row_ids, rid)
    expect_numeric(ifs$individual_fairness, len = length(rid), lower = -Inf, upper = Inf)

    l = lrn("classif.rpart", predict_types = "prob")
    l$train(t)
    prd = l$predict(t)
    ifs = individual_fairness(t$data(), prd)
    expect_set_equal(ifs$row_ids, rid)
    expect_numeric(ifs$individual_fairness, len = length(rid), lower = -Inf, upper = Inf)
})


test_that("classif - binary", {
    rid = c(4,7, 9, 10:100)
    t = tsk("compas")$filter(rid)
    l = lrn("classif.rpart")
    l$train(t)
    prd = l$predict(t)
    ifs = individual_fairness(t$data(), prd)
    expect_set_equal(ifs$row_ids, rid)
    expect_numeric(ifs$individual_fairness, len = length(rid), lower = -Inf, upper = Inf)

    l = lrn("classif.rpart", predict_type = "prob")
    l$train(t)
    prd = l$predict(t)
    ifs = individual_fairness(t$data(), prd, dX = NULL, dY = NULL, frac = 1)
    expect_set_equal(ifs$row_ids, rid)
    expect_numeric(ifs$individual_fairness, len = length(rid), lower = -Inf, upper = Inf)

    prd$score(msr("fairness.classif_individual"), task = t)
})

test_that("regr", {
    rid = c(4,7, 9, 11:30)
    t = tsk("mtcars")$filter(rid)
    l = lrn("regr.rpart")
    l$train(t)
    prd = l$predict(t)

    ifs = individual_fairness(t$data(), prd)

    expect_set_equal(ifs$row_ids, rid)
    expect_numeric(ifs$individual_fairness, len = length(rid), lower = Inf, upper = Inf)
})

test_that("surv", {
    skip_if_not_installed("mlr3proba")
    rid = c(4,7, 9, 10:100)
    t = tsk("rats")$filter(rid)

    l = lrn("surv.coxph", predict_type = "lp")
    suppressWarnings(l$train(t))
    prd = l$predict(t)
    ifs = individual_fairness(t$data(), prd)
    expect_set_equal(ifs$row_ids, rid)
    expect_numeric(ifs$individual_fairness, len = length(rid), lower = -Inf, upper = Inf)

    l = lrn("surv.glmnet", lambda = 0.01, predict_type = "lp")
    l$train(t$select(c("litter", "rx")))
    prd = l$predict(t)
    ifs = individual_fairness(t$data(), prd)
    expect_set_equal(ifs$row_ids, rid)
    expect_numeric(ifs$individual_fairness, len = length(rid), lower = -Inf, upper = Inf)

})
