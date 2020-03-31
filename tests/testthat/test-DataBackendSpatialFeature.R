test_that("initialise", {
  checkmate::expect_r6(DataBackendSpatialFeature$new(toy_zones), class = "DataBackendSpatialFeature")
})

test_that("get", {
  expect_is(DataBackendSpatialFeature$new(toy_zones)$get(), "data.table")
})

test_that("get_sf", {
  expect_is(DataBackendSpatialFeature$new(toy_zones)$get_sf(), "sf")
})

test_that("view", {
  x <- DataBackendSpatialFeature$new(toy_zones)
  if (requireNamespace("mapview", quietly = TRUE)) expect_is(x$view(interactive = TRUE), "mapview")
  # expect_null(x$view(interactive = FALSE)) # this unexpectedly prints out pdf to the test dir
})
