test_that("initialise", {
  expect_error(Entity$new(databackend = DataBackendDataTable, .data = toy_individuals, id_col = "random_name"),
               regexp = "failed: Must include the elements \\{random_name\\}")
})

test_that("data", {
  MyObj <- Entity$new(databackend = DataBackendDataTable, .data = toy_individuals, id_col = "pid")
  expect_is(MyObj$data(), class = "R6")
  expect_is(MyObj$data(), class = "DataBackendDataTable")
})

# get_data -----------
test_that("get_data", {
  MyObj <- Entity$new(databackend = DataBackendDataTable, .data = toy_individuals, id_col = "pid")
  expect_is(MyObj$get_data(), "data.table")
  expect_is(MyObj$get_data(copy = FALSE), "data.table")
  expect_true(nrow(MyObj$get_data()) == nrow(toy_individuals))
  expect_true(nrow(MyObj$get_data("attrs")) == nrow(toy_individuals))
  rand_ids <- sample(toy_individuals$pid, 10)
  expect_equal(MyObj$get_data(ids = rand_ids, copy = TRUE)[[MyObj$get_id_col()]], rand_ids)
  checkmate::expect_data_frame(MyObj$get_data(ids = c(1,1)), nrows = 2, null.ok = FALSE)

  # test modify
  MyObj$get_data()[, sex := "none"]
  checkmate::assert_subset(MyObj$get_data()[, sex], choices = c("male", "female"))

})

# add_data -----------
test_that("add_data", {
  MyObj <- Entity$new(databackend = DataBackendDataTable, .data = toy_individuals, id_col = "pid")

  expect_error(MyObj$add_data(databackend = DataBackendDataTable, .data = toy_individuals, name = "attrs"),
               regexp = "failed: Must be disjunct from \\(attrs\\)")

  MyObj$add_data(databackend = DataBackendDataTable, .data = toy_individuals, name = "attrs2")
  expect_true(all.equal(MyObj$get_data(name = "attrs2"), toy_individuals))

  Enty <- Entity$new(databackend = DataBackendDataTable, .data = toy_individuals, id_col = "pid")
})

# add -----------
test_that("add", {
  Enty <-
    Entity$new(
      databackend = DataBackendDataTable,
      .data = toy_individuals,
      id_col = c("pid", "partner_id", "mother_id", "father_id")
    )

  n_entities_before <- Enty$n()
  new_ent_dt <- data.table::copy(toy_individuals)[, .derived_col := 1]
  expect_error(Enty$add(.data = new_ent_dt, check_existing = TRUE),
               regexp = "One or more of the main unique `ids` in `.data` already exist in the existing attribute data of this Entity.")

  data_lst <- register(x = Enty, new_ent_dt)
  Enty$add(data_lst$new_ent_dt, check_existing = FALSE)
  expect_error(Enty$add(.data = new_ent_dt, check_existing = TRUE),
               regexp = "One or more of the main unique `ids` in `.data` already exist in the existing attribute data of this Entity.")

  data_lst <- register(x = Enty, new_ent_dt)
  Enty$add(data_lst$new_ent_dt, check_existing = FALSE)
  expect_equal(Enty$n(), expected = nrow(toy_individuals) * 3)

  # shuffle column order
  data_lst <- register(x = Enty, new_ent_dt)
  data.table::setcolorder(data_lst$new_ent_dt, sample(names(data_lst$new_ent_dt)))
  expect_null(Enty$add(.data = data_lst$new_ent_dt, check_existing = FALSE))

  # create newborns
  new_ent_dt <- data.table::copy(toy_individuals)[1:20, pid := 2001:2020]
  expect_null(Enty$add(new_ent_dt, check_existing = TRUE))
})

# summary ----------
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
  checkmate::expect_integerish(
    MyObj$get_ids(),
    lower = 1,
    any.missing = FALSE,
    unique = T,
    null.ok = FALSE,
    min.len = nrow(toy_individuals)
  )
})

test_that("get_idx", {
  MyObj <- Entity$new(databackend = DataBackendDataTable, .data = toy_individuals, id_col = "pid")
  expect_length(MyObj$get_idx(c(2,1,4,2)), 4)
  expect_equal(MyObj$get_idx(c(2,1,4,2)), c(2,1,4,2))
  expect_error(MyObj$get_idx(c(2,1,4,2,NA)), "Contains missing values \\(element 5\\).")
  expect_error(MyObj$get_idx(c(2,1,4,2,1000)), "These ids don't exist in Entity: 1000")
})

test_that("ids_exist", {
  MyObj <- Entity$new(databackend = DataBackendDataTable, .data = toy_individuals, id_col = "pid")
  rand_ids <- sample(MyObj$get_data()[[MyObj$get_id_col()]], 3)
  expect_true(MyObj$ids_exist(ids = rand_ids))
  expect_equal(MyObj$ids_exist(ids = c(rand_ids, 9999999)), FALSE)
  expect_error(MyObj$ids_exist(ids = NA), "Contains missing values")
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
  checkmate::expect_character(capture.output(MyObj$print_data(n = 10)))
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
  expect_error(MyObj$get_attr("abcd"), "Must be a subset of set")
  expect_error(MyObj$get_attr('age', ids = c(99999999)), regexp = "These ids don't exist in Entity: 99999999")
  checkmate::expect_integerish(MyObj$get_attr('age', ids = c(1,2,3)), lower = 0, any.missing = FALSE, len = 3, null.ok = FALSE)
})

test_that("generate_new_ids", {
  MyObj <- Entity$new(databackend = DataBackendDataTable, .data = toy_individuals, id_col = "pid")
  checkmate::expect_integerish(MyObj$get_attr(MyObj$get_id_col()), any.missing = FALSE, min.len = 1, null.ok = FALSE, unique = TRUE)
  expect_error(MyObj$get_attr("abcd"), "Must be a subset of set")
})

test_that("database", {
  MyObj <- Entity$new(databackend = DataBackendDataTable, .data = toy_individuals, id_col = "pid")
  checkmate::expect_list(MyObj$database, types = c("DataBackend"), len = 1, any.missing = FALSE, names = "strict")
})

test_that("$subset_ids", {
  Ent <-
    Entity$new(
      databackend = DataBackendDataTable,
      .data = dymiumCore::toy_individuals,
      id_col = "pid"
    )

  Ent$subset_ids(sex == "female")

  # filter non-existed column
  expect_error(Ent$subset_ids(sexp == "FEMALE"),
               regexp = "object 'sexp' not found")

  # return a vector of ids
  checkmate::expect_integerish(
    Ent$subset_ids(sex == "female"),
    lower = 1,
    any.missing = FALSE,
    unique = TRUE
  )

})
