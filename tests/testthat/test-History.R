test_that("initialise", {
  h <- History$new()
  expect_true(data.table::is.data.table(h$get_data()))

  n_entries = 5L

  for (t in 1:4) {
    h$add(
      ids = sample(1L:10L, n_entries, replace = T),
      event = sample(c("Birth", "LeaveHome", "Marriage"), n_entries, replace = T),
      time = t
    )
  }

  sample(1L:10L, n_entries, replace = T)
  sample(c("Birth", "LeaveHome", "Marriage"), n_entries, replace = T)

  h$get_data()

  expect_true(data.table::is.data.table(h$get_data()))
  expect_true(length(h$get_data()) != 0)
  expect_true(h$get_data()[, length(unlist(id))] == h$count()[, sum(N)])
  some_ids <- h$get_data()
  expect_gt(nrow(h$count(), x = 0))
  h$count(ids = 1, event = "Birth")
  expect_error(h$count(event = "nonexisted_event", "'event' failed: Must be a subset of"))


})

test_that("", {

})



# test_that("burn out", {
#   h <- History$new()
#   expect_true(data.table::is.data.table(h$get_data()))
#   entries = 1000000
#   h$add(
#     id = sample(1L:10000L, entries, replace = T),
#     time = sample(1L:50L, entries, replace = T),
#     event = sample(LETTERS, entries, replace = T))
#
#   rand_id <- h$get_data()[, unique(unlist(id))] %>% sample(10)
#
#   h$get_data()[, unique(event)]
#   data <- data.table(id = rand_id, test = "what")
#   event = "A"
#   id_col <- "id"
#   h$impute(data = data, id_col = "id", event = "A")
#
#   microbenchmark(
#     one = {h$count(id = rand_id) %>% .[]},
#     two = {h$count() %>% .[]}, unit = "s", times = 10)
# })
