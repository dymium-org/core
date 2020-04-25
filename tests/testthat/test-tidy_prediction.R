test_that("tidy_prediction.list", {
  model <- list(yes = 0.5, no = 0.5)
  tp <- tidy_prediction(model, toy_individuals)
  checkmate::expect_class(tp, classes = c("tidy_prediction", "data.table"))
  checkmate::expect_data_table(tp, any.missing = FALSE, col.names = "strict")
})

test_that("tidy_prediction.train", {
  model <- create_caret_binary_model()
  tp <- tidy_prediction(model, toy_individuals)
  checkmate::expect_class(tp, classes = c("tidy_prediction", "data.table"))
  checkmate::expect_data_table(tp, any.missing = FALSE, col.names = "strict")
})

test_that("tidy_prediction.WrappedModel", {
  model <- create_mlr_binary_model()
  tp <- tidy_prediction(model, toy_individuals)
  checkmate::expect_class(tp, classes = c("tidy_prediction", "data.table"))
  checkmate::expect_data_table(tp, any.missing = FALSE, col.names = "strict")
})

test_that("tidy_prediction.glm", {
  model <- create_glm_binary_model()
  tp <- tidy_prediction(model, toy_individuals)
  checkmate::expect_class(tp, classes = c("tidy_prediction", "data.table"))
  checkmate::expect_data_table(tp, any.missing = FALSE, col.names = "strict")
})

test_that("tidy_prediction.data.table", {
  model_static_binary <- data.table::data.table(age = rep(1:100, 2),
                                  sex = c(rep("male", 100), rep("female", 100)),
                                  prob = runif(200))
  tidy_prediction(model, toy_individuals)
  checkmate::expect_class(tp, classes = c("tidy_prediction", "data.table"))
  checkmate::expect_data_table(tp, any.missing = FALSE, col.names = "strict")

  model_static_binary <- data.table::data.table(sex = c('male', 'female'),
                                                choices = list(letters[1:2], letters[1:4]),
                                                probs = list(runif(2), runif(4)))
  tidy_prediction(model, toy_individuals)
  checkmate::expect_class(tp, classes = c("tidy_prediction", "data.table"))
  checkmate::expect_data_table(tp, any.missing = FALSE, col.names = "strict")


})
