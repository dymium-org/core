.generate_building_data <- function(n_rows){
  data.table(
    bid = 1:n_rows,
    hid = sample(c(1:8, NA, NA), n_rows),
    zid = 1L,
    price = 1000 * runif(n_rows),
    bedrooms = sample(1:4, n_rows, replace = TRUE)
  )
}

test_that("initialising a building object", {
  n_rows <- 10L
  data <- .generate_building_data(n_rows)
  id_col <- "bid"
  Bld <- Building$new(data, id_col)
  expect_true(Bld$n() == n_rows)

  Bld <- Building$new()
  data <- .generate_building_data(n_rows = n_rows)
  # alter data type to invoke error
  data[, zid := as.character(zid)]
  id_col = "hid"
  expect_error(Bld$initialise_data(data, id_col))
})

test_that("is_occupied and is_vacant", {
  n_rows <- 10L
  id_col = "bid"

  building_data <- .generate_building_data(n_rows)
  owner_data <- building_data[!is.na(hid), .(hid, bid)]


  Bld <- Building$new()
  Hh <- Household$new(.data = owner_data, id_col = "hid")

  Bld$initialise_data(building_data, id_col)
  Bld$set_owner_object(x = Hh)

  vacant <- is.na(Bld$get_attr("hid"))
  expect_equal(Bld$is_vacant(), vacant)
})
