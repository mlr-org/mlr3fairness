test_that("fairness.fpr can be loaded and work as expected", {
  pp = pred_small()
  tt = test_task_small()

  a = cbind(tt$data(cols = c(tt$col_roles$pta)), rw = tt$row_ids)
  resp = table(pp$response)
  l1 = fairness_tensor(pp, task = tt)
  l2 = fairness_tensor(pp, normalize = "none", task = tt)
  l3 = fairness_tensor(pp, normalize = "group", task = tt)

  expect_true(sum(map_int(l2, sum)) ==  length(pp$response))
  expect_true(sum(map_dbl(l1, sum)) == 1)
  expect_true(sum(map_dbl(l3, sum)) == 2)

  expect_true(all(l2[[1]] == pp$clone()$filter(a$rw[a$pta == 1])$confusion)) 
  expect_true(all(l2[[2]] == pp$clone()$filter(a$rw[a$pta == 2])$confusion))
})
