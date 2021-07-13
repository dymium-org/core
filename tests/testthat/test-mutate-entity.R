test_that("mutate_entity works", {
  create_toy_world()
  world %>%
    mutate_entity(entity = "Individual", age := age + 1L)
})

test_that("mutate_entity only works if either subset, ids or preprocessing_fn is specified but not more than one", {

  # no filering
  create_toy_world()
  world %>%
    mutate_entity(entity = "Individual", age := age + 100L)
  expect_true(all(world$entities$Individual$get_attr("age") >= 100))

  # ids
  create_toy_world()
  world %>%
    mutate_entity(entity = "Individual", age := 0L, ids = 1:3)
  expect_true(all(world$entities$Individual$get_data()[pid %in% 1:3, age] == 0))

  # subset
  create_toy_world()
  world %>%
    mutate_entity(entity = "Individual", age := 0L, subset = sex == "female")
  expect_true(all(world$entities$Individual$get_data()[sex == "female", age] == 0))

  # preprocessing_fn
  create_toy_world()
  pp_fn <- . %>% .[sex == "female"]
  world %>%
    mutate_entity(entity = "Individual", age := 0L, preprocessing_fn = pp_fn)
  expect_true(all(world$entities$Individual$get_data()[sex == "female", age] == 0))

  # error cases
  expect_error(mutate_entity(world, entity = "Individual", age := age + 1L, ids = 1:3, subset = age > 30),
    regexp = "Only one or none of the filter parameters"
  )
  expect_error(mutate_entity(world, entity = "Individual", age := age + 1L, ids = 1:3, preprocessing_fn = 1),
    regexp = "Only one or none of the filter parameters"
  )
  expect_error(mutate_entity(world, entity = "Individual", age := age + 1L, ids = 1:3, subset = age > 30, preprocessing_fn = 1),
    regexp = "Only one or none of the filter parameters"
  )
})
