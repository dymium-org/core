context('agent class')

test_that("initialise", {
  expect_error(Agent$new(.data = toy_individuals, id_col = 'non_existed_colname'))
})

test_that("initialise_data", {
  MyAgent <- Agent$new(.data = toy_individuals, id_col = 'pid')
  expect_error(MyAgent$initialise_data(.data = toy_individuals, id_col = 'pid'),
               regexp = "Agent's `attrs` data has already been initialised")
})

test_that("add_data", {
  MyAgent <- Agent$new(.data = toy_individuals, id_col = 'pid')
  expect_error(MyAgent$add_data(.data = iris, name = "iris"),
               regexp = "failed: Must include the elements \\{pid\\}",
               info = "Agent's id column should be missing.")
  MyAgent$add_data(.data = toy_individuals, name = "attrs2")
  expect_true(all(MyAgent$get_data_names() == c("attrs", "attrs2")))
})

test_that("get_removed_data", {
  MyAgent <- Agent$new(.data = toy_individuals, id_col = 'pid')
  MyAgent$add_data(.data = toy_individuals, name = "attrs2")
  expect_true(all.equal(MyAgent$get_removed_data(), toy_individuals[0, ]))
  MyAgent$remove(ids = 1:10)
  expect_true(all(MyAgent$get_data_names() == c("attrs", "attrs2")))
})

test_that("using Test data", {
  # initialise an Agent instance
  set.seed(1)
  data <- dymiumCore::toy_individuals
  n_agents <- nrow(data)
  MyAgent <- Agent$new()
  MyAgent$initialise_data(.data = data, id_col = "pid")

  # data$nrow(), get_latest_agent_id() ---------
  expect_is(MyAgent, "R6")
  expect_equal(MyAgent$get_data()[, .N], nrow(data))
  expect_equal(MyAgent$get_data()[, .N], nrow(data))
  expect_equal(object = MyAgent$get_latest_agent_id(),
               expected = MyAgent$get_data()[, max(get(MyAgent$get_id_col()))])
  expect_is(MyAgent$get_data(), "data.table")

  # get_id() ---------
  expect_length(MyAgent$get_ids(), n_agents)

  # add_new_agents() ---------
  n_new_agents <- 3L
  n_rows_before_add <- MyAgent$n()
  MyAgent$add_new_agents(parent_ids = sample(x = MyAgent$get_ids(), size = n_new_agents))
  expect_equal(n_rows_before_add, MyAgent$n() - n_new_agents)

  # remove_agent() ---------
  n_agents_to_remove <- 5L
  n_rows_before_remove <- MyAgent$n()
  MyAgent$remove(ids = sample(MyAgent$get_ids(), size = n_agents_to_remove))
  expect_equal(nrow(MyAgent$get_removed_data()), n_agents_to_remove)
  expect_equal(MyAgent$n(), n_rows_before_remove - n_agents_to_remove)

  # idx_exist() ---------
  expect_true(MyAgent$idx_exist(idx = sample(1L:n_agents, size = 3)))
  expect_false(MyAgent$idx_exist(idx = n_agents + (n_new_agents + 1) * 100))

  # get_match_ids() ---------
  obj_dt <- MyAgent$get_data()
  ids_manual <- obj_dt[sex == IND$SEX$FEMALE & age < 40, (pid)]
  ids_from_method <- MyAgent$subset_ids(sex == IND$SEX$FEMALE & age < 40)
  expect_equivalent(ids_manual, ids_from_method)

  # TODO: add test cases for get_idx() ---------

})

# #add_new_agents ----------------
test_that("add_new_agents", {
  # add by inheritance
  MyAgent <- Agent$new()
  expect_error(MyAgent$add_new_agents(parent_ids = 100),
    info = "should fail if the user did not load data")
  expect_error(MyAgent$add_new_agents(parent_ids = 100, data = data.table()),
    info = "should fail if two args are provided at the same time.")
  MyAgent$initialise_data(.data = dymiumCore::toy_individuals, id_col = "pid")
  MyAgent$add_new_agents(parent_ids = MyAgent$get_ids()[[1]])
  expect_true(nrow(MyAgent$get_data()) == nrow(dymiumCore::toy_individuals) + 1)

  # add by external data
  MyAgent <- Agent$new()
  MyAgent$initialise_data(.data = dymiumCore::toy_individuals, id_col = "pid")
  expect_error(MyAgent$add_new_agents(data = dymiumCore::toy_individuals),
    info = "All ids in `data` should be unique from ids in private$data.")
  new_ids <- MyAgent$generate_new_ids(n = nrow(dymiumCore::toy_individuals))
  lookup_table <- data.frame(key = 1:nrow(dymiumCore::toy_individuals), value = new_ids)
  new_agent_data <- lookup_and_replace(
    data = dymiumCore::toy_individuals,
    lookup_table = lookup_table,
    cols = c('pid', 'partner_id', 'mother_id', 'father_id'),
    id_col = 'pid')
  MyAgent$add_new_agents(.data = new_agent_data)
  expect_true(MyAgent$n() == nrow(dymiumCore::toy_individuals) * 2)
  pid_cols <- c('pid', 'partner_id', 'father_id', 'mother_id')
  unique_pid <- MyAgent$get_data()[, unlist(.SD), .SDcol = pid_cols] %>%
    unique() %>% na.omit() %>% length()
  expect_true(all(unique_pid %in% MyAgent$get_attr(x = MyAgent$get_id_col())))

})

test_that("is_alive", {
  data <- copy(dymiumCore::toy_individuals)
  MyAgent <- Agent$new()
  MyAgent$initialise_data(.data = data, id_col = "pid")
  expect_error(MyAgent$is_alive(ids = NA), "Contains missing values \\(element 1\\)")
  expect_error(MyAgent$is_alive(ids = MyAgent$get_attr(x = "partner_id")), "Contains missing values")
})

# $get_col ----------
test_that("Agent$get_attr", {
  MyAgent <- Agent$new()

  data <-
    structure(
      list(
        pid = 1:3,
        hid = c(1L, 1L, 1L),
        age = c(48L, 41L,
          21L),
        sex = structure(
          c(2L, 1L, 2L),
          .Label = c("FEMALE", "MALE"),
          class = "factor"
        ),
        marital_status = structure(
          c(2L, 2L, 3L),
          .Label = c(
            "DIVORCED",
            "MARRIED",
            "NEVER MARRIED",
            "NOT APPLICABLE",
            "WIDOWED"
          ),
          class = "factor"
        ),
        partner_id = c(2L, 1L, NA),
        father_id = c(NA, NA, 1L),
        mother_id = c(NA, NA, 2L),
        children_ids = list(3:5,
          3:5, integer(0))
      ),
      row.names = c(NA, -3L),
      class = c("data.table", "data.frame")
    )

  MyAgent$initialise_data(.data = data, id_col = "pid")

  expect_true(!identical(
    x = MyAgent$get_attr(x = "age", ids = 1:3),
    y = MyAgent$get_attr(x = "age", ids = c(2, 3, 1))
  ))

  expect_true(!identical(
    x = MyAgent$get_attr(x = "children_ids", ids = 1:3),
    y = MyAgent$get_attr(x = "children_ids", ids = c(2, 3, 1))
  ))
})

test_that("ids_exist", {
  create_toy_population()
  Ind <- pop$get("Individual")
  all_ids <- Ind$get_ids()
  expect_true(Ind$ids_exist(ids = all_ids, include_removed_data = FALSE))
  Ind$remove(all_ids[1:5])
  expect_false(Ind$ids_exist(all_ids, include_removed_data = FALSE))
  expect_true(Ind$ids_exist(all_ids, include_removed_data = TRUE))
})

# Agent data model --------------------------------------------------------
test_that("Agent add, get, remove and show data methods", {
  # one-to-one data -----------------
  one2one <- data.table(hid = 1:10) %>%
    .[rep(hid, sample(4, .N, replace = TRUE))] %>%
    .[, pid := 1:.N]
  MyAgent <- Agent$new(.data = one2one, id_col = "pid")
  expect_error(MyAgent$add_data(.data = one2one, name = "attrs"), regexp = "Must be disjunct from \\(attrs\\)")
  expect_true(length(MyAgent$get_data_names()) == 1)

  # doesn't contain id_col
  expect_error(MyAgent$add_data(.data = data.table(1), name = "new"), regexp = "Must include the elements \\{pid\\}")

  # can't initialise data once `attrs` has been created
  expect_error(MyAgent$initialise_data(.data = one2one, id_col = "pid"), "`attrs` data has already been initialised")


  expect_true(all(MyAgent$get_data(name = "attrs", ids = c(2,1))[['pid']] == c(2,1)))

  # error: id col doesn't exist
  expect_error(MyAgent$add_data(data.table(a = 1), id_col = NULL, name = "data_missing_id_col"))
  # no double adding
  MyAgent$add_data(.data = data.table(pid = 1), name = 'data_one')
  expect_error(MyAgent$add_data(.data = data.table(pid = 1), name = 'data_one'),
               regexp = "failed: Must be disjunct from")
  # bad names
  expect_error(MyAgent$add_data(.data = data.table(pid = 1), name = 'bad names'),
               regexp = "Must have names according to R's variable naming conventions")

  expect_error(MyAgent$add_data(.data = data.table(pid = 1), name = 'bad names1'),
               regexp = "Must have names according to R's variable naming conventions")
  # good names
  expect_null(MyAgent$add_data(.data = data.table(pid = 1), name = 'goodnames'))
  expect_null(MyAgent$add_data(.data = data.table(pid = 1), name = 'good_names'))

  # many-to-one data: many rows belong to each agent --------------
  many2one <- data.table(pid = 1:10) %>%
    .[rep(pid, sample(4, .N, replace = TRUE))] %>%
    .[, trip_id := 1:.N, by = pid]
  MyAgent$add_data(.data = many2one, name = 'trips')
  MyAgent$get_data(name = "trips", ids = c(3, 1:2))
  expect_is(MyAgent$get_data(name = "trips", ids = c(3, 1:2)), class = 'data.table')
  expect_true(nrow(MyAgent$get_data(name = "trips", ids = c(3, 1:2))) != 0)
  expect_is(MyAgent$get_data(name = "trips"),  class = 'data.table')

  # remove agents -------------------
  MyAgent$remove(ids = c(1:3))
  x <- MyAgent$summary(verbose = FALSE)
  expect_true(sum(x$nrow_removed) > 0)
})

test_that("hatch", {
  create_toy_population()
  count_before <- pop$ind$n()
  pop$ind$hatch(1)
  count_after <- pop$ind$n()
  expect_gt(count_after, count_before)
  expect_error(pop$ind$hatch(9999999), "These ids don't exist in Individual: 9999999")
})

test_that("add", {
  create_toy_population()
  count_before <- pop$ind$n()
  new_ind_data <- pop_register(pop, ind_data = toy_individuals)
  pop$ind$add(new_ind_data$ind_data)
  count_after <- pop$ind$n()
  expect_true(count_after == (count_before + nrow(toy_individuals)))
})


test_that("add new agents to existing agents with a derived variable", {
  create_toy_population()
  pop$ind$remove_relationship(sample(pop$ind$get_ids(), 50))
  pop$ind$get_data()
  new_ind_data <- pop_register(pop, ind_data = toy_individuals)
  pop$ind$add(new_ind_data$ind_data)
  expect_equal(nrow(pop$ind$get_data()), nrow(toy_individuals) * 2)
})

