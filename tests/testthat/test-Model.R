test_that("initialise", {
  x <- list(x = 1)
  m <- Model$new(x)
  expect_is(m$get(), "list")
  expect_error(Model$new(1), "Must be a subset of")
})



