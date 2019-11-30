test_that("initialize", {
  create_toy_population()
  Ind <- pop$get("Individual")
  Hh <- pop$get("Household")
  expect_error(Transition$new(1, NULL), "Must inherit from class 'Agent', but has class 'numeric'")
  a_transition <- checkmate::expect_class(Transition$new(Ind, NULL), "Transition")
  checkmate::expect_class(a_transition, "Transition")
  checkmate::expect_class(a_transition$get_result(), "data.table")
})

test_that("get_result", {
  create_toy_population()
  Ind <- pop$get("Individual")
  Hh <- pop$get("Household")
  expect_error(Transition$new(1, NULL), "Must inherit from class 'Agent', but has class 'numeric'")
  a_transition <- checkmate::expect_class(Transition$new(Ind, NULL), "Transition")
  expect_error(a_transition$get_result(ids = c(1:10, 10, 9999)), regexp = 'Must be a subset of')
  checkmate::expect_data_table(a_transition$get_result(ids = 1:10), any.missing = FALSE, nrows = 10, ncols = 2, col.names = 'strict', null.ok = FALSE)
  checkmate::expect_data_table(a_transition$get_result(ids = c(1:10, 10)), any.missing = FALSE, nrows = 11, ncols = 2, col.names = 'strict', null.ok = FALSE)
  checkmate::expect_subset(a_transition$get_result(ids = 1:10)[['id']], choices = c(1:10))
  checkmate::expect_set_equal(x = a_transition$get_result(ids = c(1:8,10,9))[['id']], y = c(1:8,10,9), ordered = T)
  checkmate::expect_class(a_transition, "Transition")
  checkmate::expect_class(a_transition$get_result(), "data.table")
})

test_that("initialise by targeted_agents", {
  create_toy_population()
  Ind <- pop$get("Individual")
  Hh <- pop$get("Household")
  idx <- 1:10
  ids <- Ind$get_data()[[Ind$get_id_col()]][idx]
  a_transition <- Transition$new(Ind, model = NULL, targeted_agents = ids)
  expect_equal(a_transition$get_result()[["id"]], ids)
})


