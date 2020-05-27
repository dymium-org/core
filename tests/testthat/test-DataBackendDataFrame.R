test_that("initialise", {
  expect_is(DataBackendDataFrame$new(data.table(x1 = c(1,2,3), x2 = c(1,2,3))), "DataBackendDataFrame")
  expect_error(DataBackendDataFrame$new(1))
})

test_that("get", {
  x <- DataBackendDataFrame$new(data.frame(x1 = c(1, 2, 3), x2 = c(1, 2, 3)))
  checkmate::expect_data_frame(x$get(),
                               ncols = 2,
                               nrows = 3)
  checkmate::expect_data_frame(x$get(1, c("x2")),
                               ncols = 1,
                               nrows = 1)
  checkmate::expect_data_frame(x$get(1:2, c("x1")),
                               ncols = 1,
                               nrows = 2)
  checkmate::expect_data_frame(x$get(1:2, c("x1", "x2")),
                               ncols = 2,
                               nrows = 2)
  checkmate::expect_data_frame(x$get(1:3),
                               ncols = 2,
                               nrows = 3)
  checkmate::expect_data_frame(x$get(cols = 1:2),
                               ncols = 2,
                               nrows = 3)
  expect_error(x$get(rows = 1:200, cols = c("x3")),
               regexp = "max of 'rows' exceeds the number of rows of the data")
  expect_equal(names(x$get(cols = c("x1", "x2"))), c("x1", "x2"))
})

test_that("DataBackendDataFrame's active fields", {
  d <- DataBackendDataFrame$new(data.table(x1 = c(1,2,3), x2 = c(1,2,3)))
  checkmate::expect_names(d$colnames, permutation.of = c("x1", "x2"))
})
