test_that("initialise", {
  x <- Asset$new()
  x$initialise_data(.data = toy_dwellings, id_col = "did")
  x$add_data(databackend = DataBackendDataTable, toy_dwellings, name = "test")
  x$get_data(copy = FALSE)[, did := NULL]
  x$get_data(copy = FALSE)[, bedroom := NULL]
})

test_that("set_owner", {
  MyAsset <- Asset$new()
  MyHousehold <- Household$new(.data = toy_households, id_col = "hid")
  MyAsset$initialise_data(.data = toy_dwellings, id_col = "did")
  MyAsset$set_owner(MyHousehold)
})
