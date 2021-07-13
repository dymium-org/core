context("population register")

test_that("pop_register works", {
  # check that all newly generated ids are not the same as the existing ids
  # for both individual and household data
  create_toy_population()
  Ind <- pop$get("Individual")
  Hh <- pop$get("Household")

  res <- pop_register(
    x = pop,
    ind_data = Ind$get_data(),
    hh_data = Hh$get_data()
  )

  ind_id_cols <- c("pid", IND$ID_COLS)
  hid_col <- "hid"

  all_new_pids <-
    res$ind_data[, unlist(lapply(.SD, unlist)), .SDcol = ind_id_cols] %>%
    unique() %>%
    .[!is.na(.)]

  all_new_hids <- res$ind[, unlist(lapply(.SD, unlist)), .SDcol = hid_col] %>%
    unique() %>%
    .[!is.na(.)]

  expect_true(
    all(!all_new_pids %in% Ind$get_ids())
  )

  expect_true(
    all(!all_new_hids %in% Hh$get_ids())
  )
})

test_that("pop_register works - individuals only", {
  # check that all newly generated ids are not the same as the existing ids
  # for both individual and household data
  create_toy_population()
  Ind <- pop$get("Individual")
  Hh <- pop$get("Household")

  res <- pop_register(
    x = pop,
    ind_data = Ind$get_data()
  )

  ind_id_cols <- c("pid", IND$ID_COLS)

  all_new_pids <-
    res$ind_data[, unlist(lapply(.SD, unlist)), .SDcol = ind_id_cols] %>%
    unique() %>%
    .[!is.na(.)]

  expect_true(
    all(!all_new_pids %in% Ind$get_ids())
  )
})
