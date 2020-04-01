test_that("mutate_entity works", {
  create_toy_world()
  world %>%
    mutate_entity(entity = "Individual", age := age + 1L)
})

test_that("mutate_entity only works if either subset or ids is specified but not both", {

  create_toy_world()
  world %>%
    mutate_entity(entity = "Individual", age := age + 100L)
  expect_true(all(world$entities$Individual$get_attr("age") >= 100))

  create_toy_world()
  world %>%
    mutate_entity(entity = "Individual", age := 0L, ids = 1:3)
  expect_true(all(world$entities$Individual$get_data()[pid %in% 1:3, age] == 0))

  create_toy_world()
  world %>%
    mutate_entity(entity = "Individual", age := 0L, subset = sex == "female")
  expect_true(all(world$entities$Individual$get_data()[sex == "female", age] == 0))

  expect_error(mutate_entity(world, entity = "Individual", age := age + 1L, ids = 1:3, subset = age > 30))
})
