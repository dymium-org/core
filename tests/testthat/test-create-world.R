test_that("create-world", {
  create_toy_world()
  expect_equal(length(world$entities), 2)
  expect_true(validate_linkages(world))
})
