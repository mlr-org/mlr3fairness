simple_autotest = function(learner, task) {
    task = task$clone(deep = TRUE)
    ft_cols = task$feature_types[, map(.SD, 1L), by = type]$id
    task$filter(seq_len(min(task$nrow, 500)))$select(cols = ft_cols)

    learner = learner$clone(deep = TRUE)
    learner$train(task)
    assert_true(!is.null(learner$model))

    for (pt in learner$predict_types) {
        learner$predict_type = pt
        prd = learner$predict(task)
        expect_class(prd, "Prediction")
    }
}