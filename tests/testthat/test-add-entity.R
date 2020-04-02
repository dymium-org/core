test_that("add_entity works", {
  world <- World$new()
  ind_data <-
    toy_individuals[, -c("hid", "father_id", "mother_id", "partner_id")]
  world$add(x = Individual$new(ind_data, id_col = "pid"))
  add_entity(world, entity = "Individual", newdata = ind_data)
  expect_equal(nrow(world$entities$Individual$get_data()),
               nrow(ind_data) * 2)
})

test_that("add_entity works with weights", {
  world <- World$new()

  ind_data <-
    toy_individuals[, -c("hid", "father_id", "mother_id", "partner_id")]

  ind_data_with_weights <-
    data.table::copy(ind_data)[, weight := runif(.N)]

  world$add(x = Individual$new(ind_data, id_col = "pid"))

  target <- 10

  add_entity(
    world,
    entity = "Individual",
    newdata = ind_data_with_weights,
    target = target,
    weight_col = "weight"
  )

  expect_equal(nrow(world$entities$Individual$get_data()),
               target + nrow(ind_data))

})

test_that("add_entity works with condition", {
  world <- World$new()

  ind_data <-
    toy_individuals[, -c("hid", "father_id", "mother_id", "partner_id")]

  ind_data_with_weights <-
    data.table::copy(ind_data)[, weight := runif(.N)]

  world$add(x = Individual$new(ind_data, id_col = "pid"))

  target <- 10

  make_true <- function() {
    TRUE
  }

  make_false <- function() {
    FALSE
  }

  make_bad_condition <- function() {
    c(TRUE, FALSE)
  }

  add_entity(
    world,
    entity = "Individual",
    newdata = ind_data_with_weights,
    target = target,
    weight_col = "weight",
    condition = make_true()
  )
  expect_equal(nrow(world$entities$Individual$get_data()),
               target + nrow(ind_data))

  add_entity(
    world,
    entity = "Individual",
    newdata = ind_data_with_weights,
    target = target,
    weight_col = "weight",
    condition = make_false()
  )
  expect_equal(nrow(world$entities$Individual$get_data()),
               target + nrow(ind_data))

  add_entity(
    world,
    entity = "Individual",
    newdata = ind_data_with_weights,
    target = target,
    weight_col = "weight",
    condition = make_bad_condition()
  )
  expect_equal(nrow(world$entities$Individual$get_data()),
               target + nrow(ind_data))

})
