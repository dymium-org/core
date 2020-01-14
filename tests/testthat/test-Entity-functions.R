test_that("add_history", {
  create_toy_world()
  n_ids <- 10000
  add_history(world$get("Individual"), ids = seq_len(n_ids), event = "test_event1", time = 1)
  add_history(world$get("Individual"), ids = seq_len(n_ids), event = "test_event2", time = 2)
  hist <- world$get("Individual")$get_data("history", copy = FALSE)
  checkmate::expect_data_table(hist, nrows = n_ids * 2)
  checkmate::assert_set_equal(hist[, unique(as.character(event))], c("test_event1", "test_event2"))
  checkmate::assert_set_equal(hist[, unique(time)], c(1:2))
  # inspectdf::inspect_mem(world$Entities$Individual$database$history$get())
  # str(world$Entities$Individual$database$history$get())
  # View(world$Entities$Individual$database$history$get())
})

test_that("inspect entities", {
  create_toy_world()
  ids <- c(1,2,3)
  inspect_res <- inspect(world$get("Individual"), ids)
  expect_null(inspect_res$entity_history)
  expect_null(inspect_res$related_entity)
  inspect_res <- inspect(world$get("Individual"), ids, related_entity = world$get("Household"))
  checkmate::expect_data_table(inspect_res$related_entity)
  expect_null(inspect_res$entity_history)
  add_history(world$get("Individual"), ids = ids, event = "hello")
  inspect_res <- inspect(world$get("Individual"), ids, world$get("Household"))
  checkmate::expect_list(inspect_res, types = c("data.table"))
  checkmate::assert_set_equal(unique(inspect_res$entity$pid), ids)
  checkmate::assert_set_equal(unique(inspect_res$entity_history$pid), ids)
})


test_that("get_history", {
  create_toy_world()
  n_ids <- 1000
  add_history(world$get("Individual"), ids = seq_len(n_ids), event = "test_event1", time = 1)
  add_history(world$get("Individual"), ids = seq_len(n_ids), event = "test_event2", time = 2)
  checkmate::expect_list(get_history(world), types = c("data.table", "NULL"))
  checkmate::expect_list(get_history(world$get("Population")), types = c("data.table", "NULL"))
  checkmate::expect_data_table(get_history(world$get("Individual")))
})
