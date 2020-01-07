test_that("initialise", {
  asset <- Asset$new()
  owner <- Household$new(.data = toy_households, id_col = "hid")
  asset$initialise_data(.data = toy_dwellings, id_col = "did", owner = owner)
  checkmate::expect_data_table(asset$get_data(), null.ok = FALSE)
  expect_equal(asset$get_owner_id_col(), owner$get_id_col())
})

test_that("set_owner_object", {
  asset <- Asset$new()
  owner <- Household$new(.data = toy_households, id_col = "hid")
  asset$initialise_data(.data = toy_dwellings, id_col = "did")
  asset$set_owner_object(owner)
  expect_equal(asset$get_owner_id_col(), owner$get_id_col())
})

test_that("remove_owner", {
  owner <- Household$new(.data = toy_households, id_col = "hid")
  asset <- Asset$new(.data = toy_dwellings, id_col = "did", owner = owner)
  asset_ids_to_be_free <- sample(asset$get_ids(), 10)
  asset$remove_owner(asset_ids_to_be_free)
  expect_true(all(is.na(asset$get_data(ids = asset_ids_to_be_free)[[owner$get_id_col()]])))
})

test_that("get_owner", {
  owner <- Household$new(.data = toy_households, id_col = "hid")
  asset <- Asset$new(.data = toy_dwellings, id_col = "did", owner = owner)
  expect_equal(asset$get_owner(ids = 1), asset$get_data(ids = 1)[[asset$get_owner_id_col()]])
  checkmate::expect_integerish(asset$get_owner(), lower = 1, any.missing = FALSE, null.ok = FALSE, len = asset$n())
})

test_that("set_owner", {
  owner <- Household$new(.data = toy_households, id_col = "hid")
  asset <- Asset$new(.data = toy_dwellings, id_col = "did", owner = owner)
  id_to_remove <- sample(asset$get_ids(), 1)
  an_owner_id <- asset$get_owner(id_to_remove)
  asset$remove_owner(id_to_remove)
  asset$set_owner(id_to_remove, an_owner_id)
  expect_equal(asset$get_owner(id_to_remove), an_owner_id)
  expect_equal(asset$owner_gets_asset_id(an_owner_id), id_to_remove)
})

test_that("is_owned", {
  owner <- Household$new(.data = toy_households, id_col = "hid")
  asset <- Asset$new(.data = toy_dwellings, id_col = "did", owner = owner)
  ids_to_remove <- sample(asset$get_ids(), 10)
  expect_true(all(asset$is_owned(ids_to_remove)))
  asset$remove_owner(ids = ids_to_remove)
  expect_true(all(asset$is_owned(ids_to_remove) == FALSE))
})
