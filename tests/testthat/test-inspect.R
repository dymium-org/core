test_that("inspect entities", {
  create_toy_world()
  ids <- c(1,2,3)
  add_history(world$Entities$Individual, ids = ids, event = "hello")
  inspect_res <- inspect(world$Entities$Individual, ids, world$Entities$Household)
  checkmate::expect_list(inspect_res, types = c("data.table"))
  checkmate::assert_set_equal(unique(inspect_res$entity$pid), ids)
  checkmate::assert_set_equal(unique(inspect_res$entity_history$pid), ids)
})
