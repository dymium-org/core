test_that("create-world", {
  create_toy_world()
  BldRes <- world$get("BuildingResidential")
  expect_equal(length(world$Entities), 4)
  expect_true(validate_linkages(world))
  checkmate::expect_r6(BldRes$get_owner_object(), "Household")
})
