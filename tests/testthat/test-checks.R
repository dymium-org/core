test_that("checks", {
  create_toy_world()
  expect_true(check_entity(world$entities$Individual))
  expect_error(assert_entity(world$containers$Population))
  expect_error(assert_entity_ids(world$entities$Individual, ids = 999:10000))
  expect_error(assert_entity_ids(world$entities$Individual, ids = 999:10000, informative = TRUE))
})
