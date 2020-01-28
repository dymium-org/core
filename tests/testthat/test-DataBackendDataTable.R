test_that("initialise", {
  expect_error(DataBackendDataTable$new(data.frame(x1 = c(1,2,3), x2 = c(1,2,3))),
               regexp = "Must be a data.table, not data.frame")
  expect_error(DataBackendDataTable$new(1),
               "Must be a data.table, not double")
})

test_that("get", {
  x <- DataBackendDataTable$new(data.table(x1 = c(1, 2, 3), x2 = c(1, 2, 3)), key = "x1")
  expect_is(x$get(), "data.table")
  expect_true(nrow(x$get(rows = 1)) == 1)
  expect_equal(names(x$get(cols = c("x1", "x2"))), c("x1", "x2"))
  expect_true(nrow(x$get()) == 3)
})

test_that("data and key", {
  x <- DataBackendDataTable$new(data.table(x1 = c(1, 2, 3), x2 = c(1, 2, 3)), key = "x1")
  expect_equal(x$key, "x1")
  expect_equal(x$get(), x$data)
})

test_that("setkey", {
  x <- DataBackendDataTable$new(data.table(x1 = c(1, 2, 3), x2 = c(1, 2, 3)))
  expect_null(x$key)
  x$setkey("x1")
  expect_equal(x$key, "x1")
  expect_equal(x$get(), x$data)
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
  expect_error(x$add(data.table(x1 = c(1,2,3), x2 = c(1,2,3), x3 = c(1,2,3))),
               "Item 2 has 3 columns, inconsistent with item 1 which has 2 columns. To fill missing columns use fill=TRUE.")
})

test_that("add fill = TRUE", {
  x <- DataBackendDataTable$new(data.table(x1 = c(1,2,3), x2 = c(1,2,3)))
  x$get(copy = FALSE)[, .test := 10]
  expect_error(x$add(data.table(x1 = c(4,5), x2 = c(4,5)), fill = FALSE),
               "Item 2 has 2 columns, inconsistent with item 1 which has 3 columns. To fill missing columns use fill=TRUE.")
  expect_error(x$add(data.table(x1 = c(4,5), x2 = c(4,5), x3 = c(1,2)), fill = FALSE),
               "Column 3 \\['x3'\\] of item 2 is missing in item 1")
  x$add(data.table(x1 = c(4,5), x2 = c(4,5), x3 = c(1,2)), fill = TRUE)
  expect_true(x$nrow() == 5)
  expect_true(x$ncol() == 4)

  expect_error(x$add(data.table(x1 = c("a","b"), x2 = c("c",5), x3 = c(1,2), .test = 10)),
               "2 string mismatches")
})
