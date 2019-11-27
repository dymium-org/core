test_that("train", {

  create_toy_population()
  Ind <- pop$get("Individual")

  # create model
  model <- caret::train(age ~ sex + marital_status, data = Ind$get_data(), method = 'glm', family = gaussian())

  # create transition
  a_transition <- TransitionRegression$new(Ind, model)

  # validate the result
  checkmate::expect_data_table(a_transition$get_result(), any.missing = FALSE, min.cols = 1, ncols = 2, null.ok = FALSE)
  checkmate::expect_names(names(a_transition$get_result()), permutation.of = c('id', 'response'))

})
