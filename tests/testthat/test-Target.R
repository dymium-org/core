test_that("Target", {
  Tgt <- Target$new(list(yes = 10))
  expect_equal(Tgt$data, list(yes = 10))
  expect_equal(Trgt$get(), list(yes = 10))
  expect_false(Tgt$dynamic)

  target_dynamic <- data.table(time = 1:10, yes = 10)
  TgtDy <- Target$new(target_dynamic)
  expect_equal(TgtDy$get(), list(yes = 10))
  expect_equal(TgtDy$data, target_dynamic)
  expect_true(TgtDy$dynamic)
})
