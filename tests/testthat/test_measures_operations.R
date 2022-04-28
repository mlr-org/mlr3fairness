test_that("core operations for fairness measures", {
  x = c(.1, .2, .15)
  expect_true(groupdiff_absdiff(x) == .1)
  expect_true(groupdiff_diff(x) == -.1)
  expect_true(groupdiff_tau(x) == .5)

  x = c(.1, .1, .15)
  expect_true(abs(groupdiff_absdiff(x) - .05) < 1e-8)
  expect_equal(groupdiff_diff(x), -0.05)
  expect_true(abs(groupdiff_tau(x) - 2 / 3) < 1e-8)

  x = c(.1, .11, 1)
  expect_true(abs(groupdiff_absdiff(x) - .9) < 1e-8)
  expect_equal(groupdiff_diff(x), -.9)
  expect_true(abs(groupdiff_tau(x) - .1) < 1e-8)

  x = c(-.1, .1, 1)
  expect_true(abs(groupdiff_absdiff(x) - 1.1) < 1e-8)
  expect_equal(groupdiff_diff(x), -1.1)
  expect_true(abs(groupdiff_tau(x) + 10.) < 1e-8)

  x = runif(5)
  expect_number(groupdiff_absdiff(x))
  expect_number(groupdiff_diff(x))
  expect_number(groupdiff_tau(x))

  x = c(1, NA)
  expect_true(is.na(groupdiff_absdiff(x)))
  expect_true(is.na(groupdiff_diff(x)))
  expect_true(is.na(groupdiff_absdiff(x)))
})
