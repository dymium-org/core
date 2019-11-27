test_that("initialise", {
  expect_error(Entity$new(databackend = DataBackendDataTable, .data = toy_individuals, id_col = "random_name"),
               regexp = "failed: Must include the elements \\{random_name\\}")
})

test_that("data", {
  MyObj <- Entity$new(databackend = DataBackendDataTable, .data = toy_individuals, id_col = "pid")
  expect_is(MyObj$data(), class = "R6")
  expect_is(MyObj$data(), class = "DataBackendDataTable")
})

test_that("get_data", {
  MyObj <- Entity$new(databackend = DataBackendDataTable, .data = toy_individuals, id_col = "pid")
  expect_is(MyObj$get_data(), "data.table")
  expect_is(MyObj$get_data(copy = FALSE), "data.table")
  expect_true(nrow(MyObj$get_data()) == nrow(toy_individuals))
  expect_true(nrow(MyObj$get_data("attrs")) == nrow(toy_individuals))
  rand_ids <- sample(toy_individuals$pid, 10)
  expect_equal(MyObj$get_data(ids = rand_ids, copy = TRUE)[[MyObj$get_id_col()]], rand_ids)
  checkmate::expect_data_frame(MyObj$get_data(ids = c(1,1)), nrows = 2, null.ok = FALSE)
})

test_that("add_data", {
  MyObj <- Entity$new(databackend = DataBackendDataTable, .data = toy_individuals, id_col = "pid")

  expect_error(MyObj$add_data(databackend = DataBackendDataTable, .data = toy_individuals, name = "attrs"),
               regexp = "failed: Must be disjunct from \\(attrs\\)")

  MyObj$add_data(databackend = DataBackendDataTable, .data = toy_individuals, name = "attrs2")
  expect_true(all.equal(MyObj$get_data(name = "attrs2"), toy_individuals))
})

test_that("summary", {
  MyObj <- Entity$new(databackend = DataBackendDataTable, .data = toy_individuals, id_col = "pid")
  expect_is(MyObj$summary(verbose = FALSE), "data.frame")
})

test_that("remove", {
  MyObj <- Entity$new(databackend = DataBackendDataTable, .data = toy_individuals, id_col = "pid")
  MyObj$add_data(databackend = DataBackendDataTable, .data = toy_individuals, name = "attrs2")
  ids_to_be_removed <- sample(MyObj$get_data()[[MyObj$get_id_col()]], 10)
  MyObj$remove(ids = ids_to_be_removed)
  expect_true(all(MyObj$summary(verbose = FALSE)[, nrow_removed] == c(10L, 10L)))
  MyObj$remove(ids = c(1,100000))
})

test_that("get_ids", {
  MyObj <- Entity$new(databackend = DataBackendDataTable, .data = toy_individuals, id_col = "pid")
  expect_true(length(MyObj$get_ids()) != 0)
  expect_true(length(MyObj$get_ids(idx = c(1:10))) == 10)
})

test_that("get_idx", {
  MyObj <- Entity$new(databackend = DataBackendDataTable, .data = toy_individuals, id_col = "pid")
  expect_length(MyObj$get_idx(c(2,1,4,2)), 4)
  expect_equal(MyObj$get_idx(c(2,1,4,2)), c(2,1,4,2))
  expect_error(MyObj$get_idx(c(2,1,4,2,NA)), "These ids do not exist")
  expect_error(MyObj$get_idx(c(2,1,4,2,1000)), "These ids do not exist")
})

test_that("ids_exist", {
  MyObj <- Entity$new(databackend = DataBackendDataTable, .data = toy_individuals, id_col = "pid")
  rand_ids <- sample(MyObj$get_data()[[MyObj$get_id_col()]], 3)
  expect_equal(MyObj$ids_exist(ids = rand_ids, by_element = T), rep(TRUE, 3))
  expect_equal(MyObj$ids_exist(ids = c(rand_ids, 9999999), by_element = T), c(rep(TRUE, 3), FALSE))
  expect_equal(MyObj$ids_exist(ids = c(rand_ids, 9999999), by_element = FALSE), FALSE)
  expect_error(MyObj$ids_exist(ids = NA, by_element = FALSE), "Contains missing values")
})

test_that("idx_exist", {
  MyObj <- Entity$new(databackend = DataBackendDataTable, .data = toy_individuals, id_col = "pid")
  rand_ids <- sample(MyObj$get_data()[[MyObj$get_id_col()]], 3)
  expect_true(MyObj$idx_exist(c(1,2,3,4)))
  expect_equal(MyObj$idx_exist(c(1,2,3,4), by_element = TRUE), rep(TRUE, 4))
  expect_equal(MyObj$idx_exist(c(1,2,3,9999999)), FALSE)
  expect_equal(MyObj$idx_exist(c(1,2,3,9999999), by_element = TRUE), c(rep(TRUE,3), FALSE))
})

test_that("print_data", {
  MyObj <- Entity$new(databackend = DataBackendDataTable, .data = toy_individuals, id_col = "pid")
  MyObj$print_data(n = 10)
})

test_that("has_attr", {
  MyObj <- Entity$new(databackend = DataBackendDataTable, .data = toy_individuals, id_col = "pid")
  expect_true(MyObj$has_attr(MyObj$get_id_col()))
  expect_true(MyObj$has_attr("abcd") == FALSE)
  expect_equal(MyObj$has_attr(c(MyObj$get_id_col(), "abcd")), c(T,F))
})


test_that("get_attr", {
  MyObj <- Entity$new(databackend = DataBackendDataTable, .data = toy_individuals, id_col = "pid")
  checkmate::expect_integerish(MyObj$get_attr(MyObj$get_id_col()), any.missing = FALSE, min.len = 1, null.ok = FALSE, unique = TRUE)
  expect_error(MyObj$get_attr("abcd"), "failed: Must include the elements \\{abcd\\}")
  expect_error(MyObj$get_attr('age', ids = c(99999999)), regexp = 'These ids do not exist')
  checkmate::expect_integerish(MyObj$get_attr('age', ids = c(1,2,3)), lower = 0, any.missing = FALSE, len = 3, null.ok = FALSE)
})

test_that("generate_new_ids", {
  MyObj <- Entity$new(databackend = DataBackendDataTable, .data = toy_individuals, id_col = "pid")
  checkmate::expect_integerish(MyObj$get_attr(MyObj$get_id_col()), any.missing = FALSE, min.len = 1, null.ok = FALSE, unique = TRUE)
  expect_error(MyObj$get_attr("abcd"), "failed: Must include the elements \\{abcd\\}")
})

