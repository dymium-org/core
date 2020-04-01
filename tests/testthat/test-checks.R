test_that("checks", {
  create_toy_world()
  expect_true(check_entity(world$entities$Individual))
  expect_error(assert_entity(world$containers$Population))
  expect_error(assert_entity_ids(world$entities$Individual, ids = 999:10000))
  expect_error(assert_entity_ids(world$entities$Individual, ids = 999:10000, informative = TRUE))

  ids_to_remove <- 1:100
  Ind <- world$entities$Individual
  Ind$remove(ids_to_remove)
  expect_error(assert_entity_ids(Ind, ids_to_remove), "These ids don't exist in Individual: 1, 2")
  checkmate::expect_r6(assert_entity_ids(Ind, ids_to_remove, include_removed_data = T), classes = "Individual")

})
