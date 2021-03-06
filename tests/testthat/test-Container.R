test_that("initialise", {
  Container$new()
})

test_that("add", {
  MyCont <- Container$new()

  MyCont$add(Agent$new(toy_individuals, "pid"), name = "Agent")
  MyCont$add(Agent$new(toy_individuals, "pid"), name = "Agents")

  expect_length(MyCont$Cont, 2)

  checkmate::expect_r6(MyCont$Cont[[1]], classes = "Agent", null.ok = FALSE)
})

test_that("get", {
  MyCont <- Container$new()

  MyCont$add(Agent$new(toy_individuals, "pid"), "Agent")

  checkmate::expect_r6(MyCont$get(1), classes = "Agent", null.ok = FALSE)

  expect_error(MyCont$get(2), "pos is greater than the number of available objecst in the container")

  expect_error(MyCont$get(-1), "pos can not be less than or equal to 0.")

  checkmate::expect_r6(MyCont$get(1), classes = "Agent")
})


test_that("remove", {
  MyCont <- Container$new()

  MyCont$add(Agent$new(toy_individuals, "pid"), "Agent1")
  MyCont$add(Agent$new(toy_individuals, "pid"), "Agent2")
  MyCont$add(Agent$new(toy_individuals, "pid"), "Agent3")

  expect_error(MyCont$remove(0), regexp = "pos can not be less than or equal to 0")
  expect_error(MyCont$remove(4), regexp = "pos is greater than the number of available objecst in the container")
  MyCont$remove(2)
  expect_length(MyCont$Cont, 2)
})
