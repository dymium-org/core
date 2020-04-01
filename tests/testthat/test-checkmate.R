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


test_that("check_subset2", {
  check_subset2(c(1, 2, 3), c(3:5))
  expect_error(assert_subset2(c(1, 2, 3), c(3:5)),
               regexp = "These element in `x` don't exist in : 1, 2")
  expect_false(test_subset2(c(1, 2, 3), c(3:5)))
  expect_subset2(c(3), c(3:5))
  checkmate::expect_integerish(assert_subset2(c(1:3), c(1:3)))
})
