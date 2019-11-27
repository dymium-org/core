context('dwelling class')

test_data <- function(n_rows){
  data <-
    data.table(
      bid = 1:n_rows,
      hid = sample(c(1:8, NA, NA), n_rows),
      zid = 1L,
      price = 1000 * runif(n_rows),
      room = sample(1:4, n_rows, replace = TRUE)
    )
  data
}

test_that("initialising a building object", {
  n_rows <- 10L
  data <- test_data(n_rows)
  id_col <- "bid"
  dw <- Building$new(data, id_col)
  expect_true(dw$n() == n_rows)

  dw <- Building$new()
  data <- test_data(n_rows = n_rows)
  # alter data type to invoke error
  data[, zid := as.character(zid)]
  id_col = "hid"
  expect_error(dw$initialise_data(data, id_col))
})

test_that("is_occupied and is_vacant", {
  n_rows <- 10L
  data <- test_data(n_rows)
  id_col = "bid"
  dw <- Building$new()
  dw$initialise_data(data, id_col)
  vacant <- is.na(dw$get_attr("hid"))
  expect_equal(dw$is_vacant(), vacant)
  expect_equal(dw$is_occupied(), !vacant)
})
