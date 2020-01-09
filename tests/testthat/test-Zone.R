test_that("initialise", {
  if (requireNamespace("sf")) {
    expect_error(Zone$new(toy_zones))
    Zn <- Zone$new(toy_zones, id_col = "zid")
    checkmate::expect_r6(Zn)
    checkmate::expect_class(Zn$data()$get_sf(), classes = "sf")
  }
})
