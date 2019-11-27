test_that("initialise", {
  expect_error(DataBackendDataTable$new(data.frame(x1 = c(1,2,3), x2 = c(1,2,3))))
  expect_error(DataBackendDataTable$new(1))
})

test_that("get", {
  x <- DataBackendDataTable$new(data.table(x1 = c(1, 2, 3), x2 = c(1, 2, 3)))
  expect_is(x$get(), "data.table")
  expect_true(nrow(x$get(rows = 1)) == 1)
  expect_equal(names(x$get(cols = c("x1", "x2"))), c("x1", "x2"))
  expect_true(nrow(x$get()) == 3)
})

test_that("remove", {
  x <- DataBackendDataTable$new(data.table(x1 = c(1,2,3), x2 = c(1,2,3)))
  x$remove(rows = 1)
  expect_true(all.equal(x$get_removed(), data.table(x1 = 1, x2 = 1)))
  expect_true(all.equal(x$get(), data.table(x1 = c(2,3), x2 = c(2,3))))
})

test_that("add", {
  x <- DataBackendDataTable$new(data.table(x1 = c(1,2,3), x2 = c(1,2,3)))
  x$add(data.table(x1 = c(4,5), x2 = c(4,5)))
  expect_true(nrow(x$get()) == 5)
  expect_error(x$add(data.table(x1 = c(1,2,3), x2 = c(1,2,3), x3 = c(1,2,3))), "have different number of columns and different column names")
})
