test_that("add", {
  w <- World$new()

  # add container
  w$add(Population$new(
    ind_data = toy_individuals,
    hh_data = toy_households,
    pid_col = c("pid"),
    hid_col = "hid"
  ))

  # change to capture the warning messages
  lg$set_threshold("warn")

  expect_output(w$add(
    Population$new(
      ind_data = toy_individuals,
      hh_data = toy_households,
      pid_col = c("pid"),
      hid_col = "hid"
    )
  ), regexp = "Replacing ")

  # add entities
  w$add(Agent$new(toy_individuals, "pid"))
  w$add(Firm$new(toy_individuals, "pid"))
  expect_length(w$entities, 4)
  expect_output(
    w$add(Individual$new(toy_individuals, "pid")),
    "Replacing"
  )
  expect_output(
    w$add(Household$new(toy_households, "hid")),
    "Replacing"
  )

  lg$set_threshold("fatal")

  # add model
  w$add(list(x = 1), "testModel")
  expect_error(w$add(list(x = 1), "badName1"), "Must comply to pattern")
  w$add(list(x = 1), "testModelTwo")
  expect_length(w$models, 2)
  w$add(Model$new(list(x = 1), "namedModel"))
  w$add(list(x = 1), "namedModel")

  # add world ?
  expect_error(w$add(w), regexp = "Adding a World object is not permitted.")
})

test_that("get", {
  w <- World$new()
  w$add(Population$new(
    ind_data = toy_individuals,
    hh_data = toy_households,
    pid_col = c("pid"),
    hid_col = "hid"
  ))
  checkmate::expect_r6(w$get("Individual"), classes = "Individual")
})

test_that("get_entity", {
  w <- World$new()
  w$add(Agent$new(toy_individuals, "pid"))
  w$add(Individual$new(toy_individuals, "pid"))
  w$add(Household$new(toy_households, "hid"))
  w$add(Firm$new(toy_individuals, "pid"))
  checkmate::expect_r6(w$get_entity("Individual"), classes = "Individual", null.ok = FALSE)
  checkmate::expect_r6(w$get_entity(Individual), classes = "Individual", null.ok = FALSE)
  checkmate::expect_r6(w$get_entity("Household"), classes = "Household", null.ok = FALSE)
  checkmate::expect_r6(w$get_entity(Household), classes = "Household", null.ok = FALSE)
  checkmate::expect_r6(w$get_entity("Agent"), classes = "Agent", null.ok = FALSE)
  checkmate::expect_r6(w$get_entity(Agent), classes = "Agent", null.ok = FALSE)
  checkmate::expect_r6(w$get_entity("Firm"), classes = "Firm", null.ok = FALSE)
  checkmate::expect_r6(w$get_entity(Firm), classes = "Firm", null.ok = FALSE)
})


test_that("get_model", {
  w <- World$new()
  w$add(list(x = 1), "goodNameOne")
  w$add(list(x = 1), "goodNameTwo")
  w$add(list(x = 1), "goodNameThree")
  w$add(data.table(x = c(1:10)), "goodNameFour")
  checkmate::expect_class(w$get_model("goodNameOne"), classes = "Model", null.ok = FALSE)
  checkmate::expect_data_table(w$get_model("goodNameFour")$get(), any.missing = FALSE, null.ok = FALSE)
  expect_error(w$get_model("good"))
})

test_that("get_container", {
  w <- World$new()
  w$add(Population$new(
    ind_data = toy_individuals,
    hh_data = toy_households,
    pid_col = c("pid"),
    hid_col = "hid"
  ))
  checkmate::expect_class(w$get_container("Population"), classes = "Population", null.ok = FALSE)
  expect_error(w$get_container("missingContainer"), "Must be element of set")
})


test_that("remove", {
  w <- World$new()
  w$add(Population$new(
    ind_data = toy_individuals,
    hh_data = toy_households,
    pid_col = c("pid"),
    hid_col = "hid"
  ))
  w$remove("Individual")
  expect_error(
    w$get("Individual"),
    "Must be element of set \\{'Household','Population'\\}"
  )
  expect_error(
    w$get("Population")$get("Individual"),
    "Must be element of set \\{'Household'\\}"
  )
  w$add(list(hi = 1), "model")
  w$remove("model")
  checkmate::expect_list(w$models, len = 0)

  w <- World$new()
  w$add(Population$new(
    ind_data = toy_individuals,
    hh_data = toy_households,
    pid_col = c("pid"),
    hid_col = "hid"
  ))
  .first_object_name <- w$names()[1]
  w$remove(1)
  expect_error(w$get(.first_object_name), regexp = "Must be element of set")
})

test_that("active fields", {
  create_toy_world()
  checkmate::expect_list(world$entities, types = "Entity", names = "strict")
  checkmate::expect_list(world$models, types = c("Model", NULL), names = "strict")
  checkmate::expect_list(world$containers, types = c("Container"), names = "strict")
})

test_that("add target", {
  t <- Target$new(x = list(yes = 10, no = 20))
  w <- World$new()
  w$add(x = t, name = "a_target")
  checkmate::expect_r6(w$targets$a_target, "Target")
})

test_that("set_scale", {
  create_toy_world()
  world_scale <- 0.01
  world$set_scale(x = world_scale)
  expect_true(world$scale == world_scale)
  world$set_scale(x = 1)
  expect_true(world$scale == 1)
  expect_error(world$set_scale(x = -1))
  expect_error(world$set_scale(x = "1"))
})
