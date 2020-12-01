test_that("World2 works", {


# constructor -------------------------------------------------------------
  world = World2$new()
  checkmate::expect_r6(world, "World2")

# add -------------------------------------------------------------------

  # accepted objects
  checkmate::expect_r6(world$add(1L, "an_integer"), "World2")
  checkmate::expect_r6(world$add(1.0, "a_number"), "World2")
  checkmate::expect_r6(world$add(list(x = 1), "a_named_list"), "World2")
  checkmate::expect_r6(world$add(Entity$new(databackend = DataBackendDataTable, .data = toy_individuals, id_col = "pid"), name = "an_entity"), "World2")
  checkmate::expect_r6(world$add(Target$new(list(x = 1)), name = "a_target"), "World2")
  checkmate::expect_r6(world$add(Model$new(list(x = 1)), name = "a_model"), "World2")

  expect_length(world$entities, 1)
  expect_length(world$properties, 5 + 3) # 5 added objects + 3 default fields (.time, .last_id, .scale)

  # not accepted objects
  expect_error(world$add(list(1), "a_list"))
  expect_error(world$add("x", "a_character"))
  expect_error(world$add(list(), "an_emptied_list"))
})

test_that("World2's S3 methods", {

  world = World2$new()

  # accepted values
  checkmate::expect_r6(set_time(world, value = 0), "World2")
  checkmate::expect_r6(set_time(world, value = 10), "World2")
  checkmate::expect_r6(set_time(world, value = 10.0), "World2")
  checkmate::expect_r6(set_time(world, value = 10L), "World2")
  checkmate::expect_r6(set_scale(world, value = 1), "World2")
  checkmate::expect_r6(set_scale(world, value = 1.5), "World2")
  checkmate::expect_r6(set_scale(world, value = 0.1), "World2")

  # not accepted values
  expect_error(set_time(world, value = 1.5))
  expect_error(set_time(world, value = -1))
  expect_error(set_scale(world, value = 0))
  expect_error(set_scale(world, value = -1))
  expect_error(set_scale(world, value = Inf))
  expect_error(set_scale(world, value = -Inf))

})

