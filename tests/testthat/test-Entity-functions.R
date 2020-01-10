test_that("add_history", {
  create_toy_world()
  n_ids <- 10000
  add_history(world$Entities$Individual, ids = seq_len(n_ids), event = "test_event1", time = 1)
  add_history(world$Entities$Individual, ids = seq_len(n_ids), event = "test_event2", time = 2)
  hist <- world$Entities$Individual$get_data("history", copy = FALSE)
  checkmate::expect_data_table(hist, nrows = n_ids * 2)
  checkmate::assert_set_equal(hist[, unique(as.character(event))], c("test_event1", "test_event2"))
  checkmate::assert_set_equal(hist[, unique(time)], c(1:2))
  # inspectdf::inspect_mem(world$Entities$Individual$database$history$get())
  # str(world$Entities$Individual$database$history$get())
  # View(world$Entities$Individual$database$history$get())
})
