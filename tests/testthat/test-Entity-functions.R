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
  # test 1
  ids <- c(1, 2, 3)
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

  # test 2
  Ind <- world$get("Individual")
  Hh <- world$get("Household")
  Dwl <- world$entities$BuildingResidential
  Zn <- world$entities$Zone
  hh_ind_data <- inspect(Hh, ids = 1:10, related_entity = Ind)
  ind_hh_data <- inspect(Ind, ids = 1:10, related_entity = Hh)
  checkmate::expect_set_equal(hh_ind_data$entity$hid, hh_ind_data$related_entity$hid)
  checkmate::expect_set_equal(ind_hh_data$entity$hid, ind_hh_data$related_entity$hid)

  # test 3
  E1 <- Entity$new(databackend = DataBackendDataTable, .data = toy_individuals, id_col = "pid")
  E2 <- Entity$new(databackend = DataBackendDataTable, .data = toy_individuals, id_col = "pid")
  inspect(E1, 1:10, E2)
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

test_that("combine_histories & plot_history", {
  create_toy_world()
  Ind <- world$get("Individual")
  Hh <- world$get("Household")
  Bd <- world$get("BuildingResidential")
  for (t in 1:10) {
    for (e in 1:5) {
      n <- sample(1:20, 1)
      add_history(Ind, ids = sample(Ind$get_ids(), n), event = sample(paste0("event-ind-", 1:5), 1), time = t)
      add_history(Hh, ids = sample(Hh$get_ids(), n), event = sample(paste0("event-hh-", 1:5), 1), time = t)
      add_history(Bd, ids = sample(Bd$get_ids(), n), event = sample(paste0("event-bd-", 1:5), 1), time = t)
    }
  }
  chist <- combine_histories(world)
  checkmate::expect_data_table(chist)
  checkmate::expect_names(names(chist), identical.to = c("time", "created_timestamp", "event", "id", "entity"))
  if (requireNamespace("patchwork")) {
    checkmate::expect_class(plot_history(world, by_entity = TRUE), classes = "ggplot")
    checkmate::expect_class(plot_history(world, by_entity = FALSE), classes = "ggplot")
    checkmate::expect_class(plot_history(chist, by_entity = TRUE), classes = "ggplot")
    checkmate::expect_class(plot_history(chist, by_entity = FALSE), classes = "ggplot")
  }
})
