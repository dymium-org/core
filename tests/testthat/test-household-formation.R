context("household formations")

test_that("household_formation - join household", {
  create_toy_population()
  Ind <- pop$get("Individual")
  Hh <- pop$get("Household")

  # test data for join household
  ind_id_list <- list(c(sample(Ind$get_ids(), 3)), c(sample(Ind$get_ids(), 2)))
  mapping <- data.table(
    ind_id = ind_id_list,
    hh_id = c(1,2)
  )
  pop$leave_household(ind_ids = mapping[, unlist(ind_id)])
  household_formation(pop, mapping = mapping, type = "join")
  expect_true(
    all(Ind$get_household_ids(ids = unlist(ind_id_list)) == c(1,1,1,2,2))
  )
})

test_that("household_formation:new household", {
  create_toy_population()
  Ind <- pop$get("Individual")
  Hh <- pop$get("Household")

  # test data for new household
  partner_x <- c(1:10)
  partner_y <- c(11:20)
  couple_ids <- purrr::map2(partner_x, partner_y, ~ c(.x, .y))
  mapping <- data.table(
    ind_id = couple_ids,
    hh_id = NA_integer_
  )

  hh_ids_before_move <-
    Ind$get_household_ids(unlist(couple_ids))

  pop$leave_household(ind_ids = mapping[, unlist(ind_id)])
  household_formation(pop, mapping = mapping, type = "new")

  expect_true(
    all(Ind$get_household_ids(ids = unlist(couple_ids)) != hh_ids_before_move)
  )
})

test_that("household_formation:random join household", {
  create_toy_population()
  Ind <- pop$get("Individual")
  Hh <- pop$get("Household")

  # test data for join household
  ind_id_list <- list(c(1,2,3), c(4,5))
  mapping <- data.table(
    ind_id = ind_id_list,
    hh_id = NA_integer_
  )

  hh_ids_before_move <-
    Ind$get_household_ids(unlist(ind_id_list))

  pop$leave_household(ind_ids = mapping[, unlist(ind_id)])
  household_formation(pop, mapping = mapping, type = "randomjoin")

  expect_true(
    !all(Ind$get_household_ids(ids = unlist(ind_id_list)) %in% hh_ids_before_move)
  )
})
