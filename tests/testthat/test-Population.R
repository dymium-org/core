context("population class")

# initialise_data2 tests ---------------------------------------------------
test_that("$initialise_data", {
  create_toy_population()

  # missing household by removing
  pop <- Population$new()
  toy_households2 <-
    copy(dymiumCore::toy_households)[-1,]
  expect_error(
    pop$initialise_data(
      ind_data = dymiumCore::toy_individuals,
      hh_data = toy_households2
    ), regexp = "Some ids in `hid_col` are not linkable between `ind_data` or `hh_data`. Please check for missing ids."
  )

  # missing household by altering ids
  pop <- Population$new()
  toy_households2 <-
    copy(dymiumCore::toy_households)[1, hid := 99999]
  expect_error(
    pop$initialise_data(
      ind_data = dymiumCore::toy_individuals,
      hh_data = toy_households2
    ), regexp = "Some ids in `hid_col` are not linkable between `ind_data` or `hh_data`. Please check for missing ids."
  )

  # missing individual in ind_data by removing
  pop <- Population$new()
  toy_individuals2 <- copy(dymiumCore::toy_individuals)[-1, ]
  expect_error(
    pop$initialise_data(
      ind_data = toy_individuals2,
      hh_data = dymiumCore::toy_households
    ), regexp = "Some ids in `hid_col` are not linkable between `ind_data` or `hh_data`. Please check for missing ids."
  )

})

# add_population -----------------------------------------------------------
test_that("add_population", {
  create_toy_population()

  toy_individuals_new_ids <- pop_register(
    x = pop,
    ind_data = dymiumCore::toy_individuals,
    hh_data = dymiumCore::toy_households
  )

  pop$add_population(
    ind_data = toy_individuals_new_ids$ind_data,
    hh_data = toy_individuals_new_ids$hh_data
  )

  # check unique
  expect_true(
    all(
      unique(pop$get('Individual')$get_data()[, get(pop$get('Individual')$get_hid_col())]) %in%
        pop$get('Household')$get_attr(x = pop$get('Household')$get_id_col())
    ), info = "Not all hh ids exist across hh and ind data")

  # add new individuals with no hh_data
  newborns <-
    data.table::copy(toy_individuals_new_ids$ind_data) %>%
    .[, `:=`(pid = sample(10000:100000, .N, replace = FALSE),
             age = 0)]

  nrow_before <- nrow(pop$get('Individual')$get_data())
  pop$add_population(ind_data = newborns)
  nrow_after <- nrow(pop$get('Individual')$get_data())
  expect_true(nrow_after > nrow_before)

  # add new individuals with non-existed household id
  newborns[1, c(pop$get('Individual')$get_hid_col()) := -1L]
  expect_error(pop$add_population(ind_data = newborns), regexp = "Assertion on 'ids' failed")
})

# remove_population -------------------------------------------------------
test_that("remove_population", {
  create_toy_population()
  expect_error(pop$remove_population())

  # remove households
  hid_to_be_removed <- c(1L, 2L)
  pid_to_be_removed <-
    pop$get('Individual')$get_ids_in_hids(hids = hid_to_be_removed)
  pop$remove_population(hid = hid_to_be_removed)
  expect_true(all(
    !pid_to_be_removed %in% pop$get('Individual')$get_attr(x = pop$get('Individual')$get_id_col())
  ))
  expect_true(all(
    !hid_to_be_removed %in% pop$get('Household')$get_attr(x = pop$get('Household')$get_id_col())
  ))

  # remove individuals
  pid_to_be_removed <- c(13, 14, 15)
  pop$remove_population(pid = pid_to_be_removed)
  expect_true(all(!pid_to_be_removed %in% pop$get('Individual')$get_data()[, pid]))
  expect_true(all(pid_to_be_removed %in% pop$get('Individual')$get_removed_data()[, pid]))
})

# $get_hhsize ----------
test_that("$get_hhsize", {
  create_toy_population()

  # remove individuals and check sum before and after
  sum_hhsize_before <- sum(pop$get_hhsize())
  pop$remove_population(pid = c(1:6))
  sum_hhsize_after <- sum(pop$get_hhsize())
  expect_true(sum_hhsize_after < sum_hhsize_before)
})

# $update_hhsize ----------
test_that("$update_hhsize", {
  create_toy_population()

  # remove individuals and check sum before and after
  sum_hhsize_before <- sum(pop$get_hhsize())
  pop$remove_population(pid = c(1:6))
  sum_hhsize_after <- sum(pop$get_hhsize())
  expect_true(sum_hhsize_after < sum_hhsize_before)

  # update hhsize
  pop$update_hhsize()
})

# check_unique_id_cols =============
test_that("check_unique_id_cols", {
  create_toy_population()

  expect_error(
    pop$check_unique_id_cols(
      ind_data = dymiumCore::toy_individuals,
      hh_data = dymiumCore::toy_households
    )
  )

  expect_true(
    pop$check_unique_id_cols(
      ind_data = copy(dymiumCore::toy_individuals)[, `:=`(pid = 9999, hid = NA_integer_)]
      )
  )

  expect_error(
    pop$check_unique_id_cols(
      ind_data = copy(dymiumCore::toy_individuals)[, `:=`(pid = 9999)],
      hh_data = copy(dymiumCore::toy_households)[, hid := 9999]
    )
  )

  expect_true(
    pop$check_unique_id_cols(
      ind_data = copy(dymiumCore::toy_individuals)[, `:=`(pid = 9999, hid = 9999)],
      hh_data = copy(dymiumCore::toy_households)[, hid := 9999]
    )
  )
})

test_that("remove_emptied_households", {
  create_toy_population()

  Hh <- pop$get("Household")
  Ind <- pop$get("Individual")

  n_hh_before <- Hh$n()
  n_ind_before <- Ind$n()

  Ind$remove(ids = 1:100)

  pop$remove_emptied_households(update_hhsize = TRUE)

  n_hh_after <- Hh$n()
  n_ind_after <- Ind$n()

  expect_gt(n_hh_before, n_hh_after)
  expect_gt(n_ind_before, n_ind_after)
})
