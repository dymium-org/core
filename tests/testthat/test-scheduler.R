test_that("scheduler", {
  expect_true(is_scheduled(time_steps = 0))
  expect_false(is_scheduled(time_steps = 1))
})
