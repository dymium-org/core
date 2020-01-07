test_that("initialise", {
  e <- Environment$new(.data = toy_network, id_col = "id")
  expect_error(Environment$new(.data = toy_network, id_col = "length"))
  checkmate::expect_data_table(e$get_data())
})
