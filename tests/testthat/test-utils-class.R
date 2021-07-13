test_that("get_log", {
  create_toy_world()
  world$entities$Individual$log(desc = "hello", value = "world")
  world$entities$Household$log(desc = "hello", value = "world")
  world$entities$BuildingResidential$log(desc = "hello", value = "world")
  world$entities$Zone$log(desc = "hello", value = "world")
  checkmate::expect_data_table(get_log(world), nrows = 4, ncols = 6)
  checkmate::expect_data_table(get_log(world$entities$Individual), nrows = 1, ncols = 6)
})

test_that("register", {
  create_toy_world()
  world$entities$Individual$get_data()
  res <- register(world$entities$Household, toy_households, toy_individuals)
  checkmate::assert_data_table(res[[1]], nrows = nrow(toy_households))
  checkmate::assert_data_table(res[[2]], nrows = nrow(toy_individuals))
  # register2(list(world$entities$Household,
  #               world$entities$Individual),
  #          list(toy_households, toy_individuals))
  # {register(x = world$entities$Individual, .)}
  # world$entities$Individual$add(.data = new_ind_data)
})

test_that("omit_derived_vars", {
  x <- data.frame(a = 1, b = 2, .c = 3)
  checkmate::assert_names(names(omit_derived_vars(x)), permutation.of = c("a", "b"))
  expect_equal(omit_derived_varnames(x), c("a", "b"))
})
