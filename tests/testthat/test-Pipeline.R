test_that("initialise", {

  event1 <- function(x) {
    # cat("event1", sep = "\n")
    x
  }

  event2 <- function(x) {
    # cat("event2", sep = "\n")
    x
  }

  pipeline <- Pipeline$new()

  pipeline$set(
    x = . %>% event1 %>% event2
  )

  expect_is(pipeline$run(Container$new()), "Container")
  expect_is(pipeline$run(Container$new(), shuffle = TRUE, n_loops = 10), "Container")

})



