test_that("get_log", {
  create_toy_world()
  world$entities$Individual$log(desc = "hello", value = "world")
  world$entities$Household$log(desc = "hello", value = "world")
  world$entities$BuildingResidential$log(desc = "hello", value = "world")
  world$entities$Zone$log(desc = "hello", value = "world")
  checkmate::expect_data_table(get_log(world), nrows = 4, ncols = 6)
  checkmate::expect_data_table(get_log(world$entities$Individual), nrows = 1, ncols = 6)
})
