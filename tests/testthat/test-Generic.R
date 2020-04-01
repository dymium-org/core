test_that("log", {
  g <- Generic$new()
  some_function <- function() {
    g$log(desc = "hello", value = list(1, 2, 3))
    g$log(desc = "hello", value = 10)
    g$log(desc = "hello", value = "hi")
    g$log(desc = "hello", value = "hi", tag = "hello")
    g$log(desc = "hello", value = "hi", tag = 9999)
  }
  expect_error(some_function())
  g$debug()
  checkmate::expect_data_table(
    private$.log,
    nrows = 4,
    ncols = 6,
    types = c("list", "integer", "character")
  )
})
