test_that("initialise", {
  w <- World$new()
})


test_that("add", {
  w <- World$new()

  # add container
  w$add(Population$new())
  expect_error(w$add(Population$new()), "Individual already exists in .entities")

  # add entities
  w$add(Agent$new())
  w$add(Firm$new())
  expect_length(w$entities, 4)
  expect_error(w$add(Individual$new()), "Individual already exists in .entities")
  expect_error(w$add(Household$new()), "Household already exists in .entities")

  # add model
  w$add(list(x = 1), "testModel")
  expect_error(w$add(list(x = 1), "badName1"), "Must comply to pattern")
  w$add(list(x = 1), "testModelTwo")
  expect_length(w$models, 2)
})

test_that("get", {
  w <- World$new()
  w$add(Population$new())
  checkmate::expect_r6(w$get("Individual"), classes = "Individual")
})

test_that("get_entity", {
  w <- World$new()
  w$add(Agent$new())
  w$add(Individual$new())
  w$add(Household$new())
  w$add(Firm$new())
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
  w$add(Population$new())
  checkmate::expect_class(w$get_container("Population"), classes = "Population", null.ok = FALSE)
  expect_error(w$get_container("missingContainer"), "Must be element of set")
})


test_that("remove", {
  w <- World$new()
  w$add(Population$new())
  w$remove("Individual")
  expect_error(w$get("Individual"),
               "Must be element of set \\{'Household','Population'\\}")
  expect_error(w$get("Population")$get("Individual"),
               "Must be element of set \\{'Household'\\}")

  w <- World$new()
  w$add(Population$new())
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
