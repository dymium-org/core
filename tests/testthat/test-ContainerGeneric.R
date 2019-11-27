test_that("initialise", {
  ContainerGeneric$new()
})

test_that("add", {
  MyCont <- ContainerGeneric$new()

  MyCont$add(Agent$new())
  MyCont$add(Agent$new())

  expect_length(MyCont$Cont, 2)

  checkmate::expect_r6(MyCont$Cont[[1]], classes = "Agent", null.ok = FALSE)
})

test_that("get", {
  MyCont <- ContainerGeneric$new()

  MyCont$add(Agent$new())

  checkmate::expect_r6(MyCont$get(pos = 1), classes = "Agent", null.ok = FALSE)

  expect_error(MyCont$get(pos = 2), "pos is greater than the number of available objecst in the container")

  expect_error(MyCont$get(pos = -1), "pos can not be less than or equal to 0.")

  MyCont$get(1)
})


test_that("remove", {
  MyCont <- ContainerGeneric$new()

  MyCont$add(Agent$new())
  MyCont$add(Agent$new())
  MyCont$add(Agent$new())

  expect_error(MyCont$remove(pos = 0), regexp = "pos can not be less than or equal to 0")
  expect_error(MyCont$remove(pos = 4), regexp = "pos is greater than the number of available objecst in the container")
  MyCont$remove(2)
  expect_length(MyCont$Cont, 2)

})
