test_that("add_history", {
  create_toy_world()
  add_history(world$Entities$Individual, ids = c(1:1000000), event = "test_event", time = 1)
  inspectdf::inspect_mem(world$Entities$Individual$get_data("history"))
})
