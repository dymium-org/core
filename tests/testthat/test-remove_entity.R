test_that("remove_entity", {

  create_toy_world()
  ind <- world$get("Individual")
  remove_entity(world, "Individual", age < 10)
  expect_true(nrow(ind$get_data()[age < 10, ]) == 0)
  expect_true(nrow(ind$get_removed_data()[age < 10, ]) != 0)


  # works when including a global variable
  create_toy_world()
  ind <- world$get("Individual")
  age_max <- 20
  remove_entity(world, "Individual", age < age_max)
  expect_true(nrow(ind$get_data()[age < age_max, ]) == 0)
  expect_true(nrow(ind$get_removed_data()[age < age_max, ]) != 0)

})
