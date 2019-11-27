context('history class')
test_that("create history class", {
  h <- History$new()
  expect_true(data.table::is.data.table(h$get_data()))
  h$add(
    id = sample(1L:10L, 20, replace = T),
    time = sample(1L:4L, 20, replace = T),
    event = sample(c("Birth", "LeaveHome", "Marriage"), 20, replace = T))
  expect_true(data.table::is.data.table(h$get_data()))
  expect_true(length(h$get_data()) != 0)
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
