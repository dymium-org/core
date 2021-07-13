test_that("dm_get_model", {
  create_toy_world()
  testModel <- list(a = 1)
  world$add(testModel, "testModel")
  expect_error(dm_get_model(world, c("testModel", "missingModel")),
    regexp = "Must be a subset of"
  )
  checkmate::expect_list(dm_get_model(world, c("testModel", "testModel")), any.missing = FALSE, len = 2)
})
