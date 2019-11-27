test_that("initialise", {
  x <- DataBackendSpatialFeature$new(toy_zones)
})

test_that("get", {
  x <- DataBackendSpatialFeature$new(toy_zones)
  expect_is(x$get(), "data.table")
})

test_that("get_sf", {
  x <- DataBackendSpatialFeature$new(toy_zones)
  expect_is(x$get_sf(), "sf")
})

test_that("view", {
  x <- DataBackendSpatialFeature$new(toy_zones)
  if (requireNamespace("mapview", quietly = TRUE)) expect_is(x$view(interactive = TRUE), "mapview")
  # expect_null(x$view(interactive = FALSE)) # this unexpectedly prints out pdf to the test dir
})
