
test_that("explicit pta", {
    skip_on_cran()
    t = tsk("adult_train")
    p = po("explicit_pta")
    nt = p$train(list(t))[[1]]
    expect_true(p$is_trained)
    expect_equal(nt$feature_names, t$feature_names)
    expect_equal(nt$col_roles$pta, "..internal_pta_sex")

    nt = p$predict(list(t))[[1]]
    expect_equal(nt$feature_names, t$feature_names)
    expect_equal(nt$col_roles$pta, "..internal_pta_sex")
})

test_that("explicit pta prevents from dropping during encode", {
    skip_on_cran()
    t = tsk("adult_train")
    p = po("explicit_pta") %>>% po("encodeimpact")
    nt = p$train(t)[[1]]
    expect_true(p$is_trained)
    expect_true(all(nt$feature_types$type %in% c("integer", "numeric")))
    expect_equal(nt$col_roles$pta, "..internal_pta_sex")
    npta = nt$data(cols = nt$col_roles$pta)[[1]]
    opta = t$data(cols = t$col_roles$pta)[[1]]
    expect_equal(npta, opta)

    nt = p$predict(t)[[1]]
    expect_true(p$is_trained)
    expect_true(all(nt$feature_types$type %in% c("integer", "numeric")))
    expect_equal(nt$col_roles$pta, "..internal_pta_sex")
    npta = nt$data(cols = nt$col_roles$pta)[[1]]
    opta = t$data(cols = t$col_roles$pta)[[1]]
    expect_equal(npta, opta)
})
