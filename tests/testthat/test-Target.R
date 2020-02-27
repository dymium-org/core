#' make sure no funny businesses in other tests
options(dymium.simulation_scale = 1)

test_that("Static target creation", {
  #' list
  Tgt <- Target$new(list(yes = 10))
  expect_equal(Tgt$data, list(yes = 10))
  expect_equal(Tgt$get(), list(yes = 10))
  expect_false(Tgt$dynamic)
})

test_that("Dynamic target creation", {
  target_dynamic <- data.table::data.table(time = 1:10, yes = 10, no = 20)
  TgtDy <- Target$new(target_dynamic)
  expect_equal(TgtDy$get(), list(yes = 10, no = 20))
  expect_equal(TgtDy$data, target_dynamic)
  expect_true(TgtDy$dynamic)
  expect_target(TgtDy)
})

test_that("scale works with targets", {
  #' scale down all targets
  options(dymium.simulation_scale = 0.1)

  Tgt <- Target$new(list(yes = 10))
  expect_equal(Tgt$data, list(yes = 10))
  expect_equal(Tgt$get(), list(yes = 1))
  expect_false(Tgt$dynamic)

  target_dynamic <- data.table::data.table(time = 1:10, yes = 10, no = 20)
  TgtDy <- Target$new(target_dynamic)
  expect_equal(TgtDy$get(), list(yes = 1, no = 2))
  expect_equal(TgtDy$data, target_dynamic)
  expect_true(TgtDy$dynamic)
  expect_target(TgtDy)

  #' set back to 1, if this is not done then errors will be raised in other parts
  #' that don't expect target to be scaled down!!
  options(dymium.simulation_scale = 1)
})
