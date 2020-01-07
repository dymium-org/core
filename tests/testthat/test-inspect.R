test_that("inspect entities", {
  create_toy_world()
  ids <- c(1,2,3)
  world$Entities$Individual$history$add(id = ids, event = "hello")
  checkmate::expect_list(
    inspect(world$Entities$Individual, ids, world$Entities$Household),
    types = c("data.table")
  )
})
