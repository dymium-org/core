test_that("check_target", {

  expect_true(check_target(NULL))

  expect_error(check_target(1))

  expect_error(assert_target(1))

  expect_error(assert_target(list(1)))

  expect_true(check_target(list(yes = 1)))

  expect_error(check_target(data.frame(1)))

  expect_error(check_target(data.frame(time = 1:10)))

  expect_error(check_target(data.frame(time = 1:10)))

  expect_error(check_target(data.frame(time = rep(1, 10))),
               regexp = "Must have at least 2 cols, but has 1")

  expect_error(check_target(data.frame(time = paste(1:10))))

  expect_true(check_target(data.frame(time = 1:10, yes = 1:10)))

})
