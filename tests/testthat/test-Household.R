test_that("Household$add", {
  create_toy_population()
  Hh <- pop$get("Household")
  n_new_hh <- 10
  n_before <- Hh$n()
  Hh$add(n = n_new_hh)
  n_after <- Hh$n()
  expect_error(Hh$add(n = -n_new_hh))
  expect_equal(n_before + n_new_hh, n_after)
})
