test_that("event", {
  expect_error(use_event("x"), "is missing, with no default")
  expect_error(use_event("x", "y"), "A module called 'y' doesn't exist")
})
