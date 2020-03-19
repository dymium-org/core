test_that("validate_linkages", {
  create_toy_world()
  expect_true(validate_linkages(world))
  Ind <- world$get_entity("Individual")
  # invalidate hid for the first record of Individual
  Ind$get_data(copy = FALSE)[1, hid := 9999]
  expect_error(validate_linkages(world), "Not all entries in Household exists in Individual")
})
