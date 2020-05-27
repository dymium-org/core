context("population class")

test_that("initialize", {
  # missing household by removing
  create_toy_population()
  toy_households2 <- copy(dymiumCore::toy_households)[-1,]

  expect_error(
    pop$add_population(ind_data = dymiumCore::toy_individuals,
                       hh_data = toy_households2),
    regexp = "Not all household ids exist in both `ind_data` and `hh_data`."
  )

  # missing household by altering ids
  create_toy_population()
  toy_households2 <-
    copy(dymiumCore::toy_households)[1, hid := 99999]

  expect_error(
    pop$add_population(ind_data = dymiumCore::toy_individuals,
                       hh_data = toy_households2),
    regexp = "Not all household ids exist in both `ind_data` and `hh_data`."
  )

  # missing individual in ind_data by removing
  create_toy_population()
  toy_individuals2 <- copy(dymiumCore::toy_individuals)[-1, ]
  expect_error(
    pop$add_population(ind_data = toy_individuals2,
                       hh_data = dymiumCore::toy_households),
    regexp = "Not all household ids exist in both `ind_data` and `hh_data`."
  )

})

# add_population -----------------------------------------------------------
test_that("add_population", {
  create_toy_population()
  Ind <- pop$get("Individual")
  Hh <- pop$get("Household")
  pop$add_population(ind_data = toy_individuals, hh_data = toy_households)
  checkmate::expect_integerish(Ind$get_attr("pid"), any.missing = FALSE, lower = 1, unique = T)
  checkmate::expect_integerish(Ind$get_attr("partner_id"), lower = 1, any.missing = TRUE)
  checkmate::expect_integerish(Ind$get_attr("mother_id"), lower = 1, any.missing = TRUE)
  checkmate::expect_integerish(Ind$get_attr("father_id"), lower = 1, any.missing = TRUE)
  checkmate::expect_subset(na.omit(Ind$get_attr("partner_id")), Ind$get_attr("pid"))
  checkmate::expect_subset(na.omit(Ind$get_attr("mother_id")), Ind$get_attr("pid"))
  checkmate::expect_subset(na.omit(Ind$get_attr("father_id")), Ind$get_attr("pid"))
  checkmate::expect_integerish(Hh$get_attr("hid"), any.missing = FALSE, lower = 1, unique = T)
  checkmate::expect_subset(Ind$get_attr("hid"), Hh$get_attr("hid"))
  checkmate::expect_subset(Ind$get_attr("hid"), Hh$get_attr("hid"))
  checkmate::expect_subset(Ind$get_attr("hid"), Hh$get_attr("hid"))

  # add new individuals with no hh_data
  newborns <-
    data.table::copy(toy_individuals) %>%
    .[, `:=`(pid = sample(10000:20000, .N, replace = FALSE),
             age = 0)]

  Ind$add(newborns, check_existing = TRUE)

  # add new individuals with non-existed household id
  newborns <-
    data.table::copy(toy_individuals) %>%
    .[, `:=`(pid = sample(30000:40000, .N, replace = FALSE),
             age = 0)] %>%
    .[1, hid := 999999999]

  expect_error(Ind$add(newborns, check_existing = TRUE),
               "These element in `x` don't exist in : 999999999")
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

  pop$check_unique_id_cols(ind_data = copy(dymiumCore::toy_individuals)[, `:=`(pid = 9999, hid = NA_integer_)])

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

test_that("`household_type` of two random hid vectors of the same set be equipvalent.", {
  create_toy_world()
  Pop <- world$get("Population")
  expect_true(
    all(table(Pop$household_type(hid = sample(1:100))) == table(Pop$household_type(hid = sample(1:100))))
  )
})
