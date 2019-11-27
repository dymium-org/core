context('individual class')

test_that("individual class's methods", {
  create_toy_population()
  Ind <- pop$get("Individual")

# is_alive ----------------------------------------------------------------

  expect_equal(object = Ind$is_alive(c(Ind$get_ids()[1], 99999999, Ind$get_ids()[1])),
               expected = c(TRUE, FALSE, TRUE))

# get_children functions --------------------------------------------------

  rand_ids <- sample(Ind$get_ids(), 10, replace = FALSE)

  expect_true(length(Ind$get_children(ids = c(rand_ids))) == length(rand_ids))

  # NA shouldn't work
  expect_error(Ind$get_children(ids = c(rand_ids ,NA)))

  # id doesn't exist
  expect_error(Ind$get_resident_children(ids = c(rand_ids ,9999999)))

# living_together ---------------------------------------------------------
  expect_is(object = Ind$living_together(c(NA, sample(rand_ids)[2:10]), c(sample(rand_ids)[1:9], NA)),
            class = "logical")
})
