test_that("initialise", {
  asset <- Asset$new()
  owner <- Household$new(.data = toy_households, id_col = "hid")
  asset$initialise_data(.data = toy_dwellings, id_col = "did", owner = owner)
  checkmate::expect_data_table(asset$get_data(), null.ok = FALSE)
  expect_equal(asset$get_owner_id_col(), owner$get_id_col())
})

test_that("set_owner", {
  asset <- Asset$new()
  owner <- Household$new(.data = toy_households, id_col = "hid")
  asset$initialise_data(.data = toy_dwellings, id_col = "did")
  asset$set_owner(owner)
  expect_equal(asset$get_owner_id_col(), owner$get_id_col())
})
