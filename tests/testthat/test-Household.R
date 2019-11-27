test_that("household's methods", {
  create_toy_population()
  Hh <- pop$get("Household")
  n_new_hh <- 10
  n_before <- Hh$n()
  Hh$add_new_agents(n = n_new_hh)
  n_after <- Hh$n()
  expect_error(Hh$add_new_agents(n = -n_new_hh))
  expect_equal(n_before + n_new_hh, n_after)
})
