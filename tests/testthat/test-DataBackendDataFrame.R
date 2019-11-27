test_that("initialise", {
  expect_is(DataBackendDataFrame$new(data.table(x1 = c(1,2,3), x2 = c(1,2,3))), "DataBackendDataFrame")
  expect_error(DataBackendDataFrame$new(1))
})

test_that("get", {
  x <- DataBackendDataFrame$new(data.frame(x1 = c(1, 2, 3), x2 = c(1, 2, 3)))
  expect_is(x$get(), "data.frame")
  expect_true(nrow(x$get(rows = 1)) == 1)
  expect_equal(names(x$get(cols = c("x1", "x2"))), c("x1", "x2"))
  expect_true(nrow(x$get()) == 3)
})
