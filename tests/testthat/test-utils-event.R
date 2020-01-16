test_that("scheduler", {
  expect_true(is_scheduled(time_steps = 0))
  expect_false(is_scheduled(time_steps = 1))
})


test_that("check_required_models", {
  # find one model
  create_toy_world()
  my_model <- list(model_two = list(yes = 0.1, no = 0.9))
  world$add(x = list(yes = 0.5, no = 0.5), "model_one")
  world$add(x = list(yes = 0.5, no = 0.5), "model_two")
  REQUIRED_MODELS <- c("model_two")
  expect_true(check_required_models(my_model, REQUIRED_MODELS))
  REQUIRED_MODELS <- c("model_one", "model_two")
  expect_error(assert_required_models(my_model, REQUIRED_MODELS),
               "These required models are not present: model_one.")
})
