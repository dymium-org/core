test_that("scheduler", {
  checkmate::expect_flag(is_scheduled(time_steps = 0), na.ok = FALSE)
  checkmate::expect_flag(is_scheduled(time_steps = 1), na.ok = FALSE)
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
  expect_error(
    assert_required_models(my_model, REQUIRED_MODELS),
    "These required models are not present: model_one."
  )
})

test_that("pick_models - 1", {
  # find one model
  create_toy_world()
  my_model <- list(model_two = list(yes = 0.1, no = 0.9))
  world$add(x = list(yes = 0.5, no = 0.5), "model_one")
  world$add(x = list(yes = 0.5, no = 0.5), "model_two")
  REQUIRED_MODELS <- c("model_one", "model_two")
  checkmate::expect_list(
    pick_models(my_model, world, REQUIRED_MODELS),
    types = SupportedTransitionModels(),
    any.missing = FALSE
  )
})

test_that("pick_models - 2", {
  # find one model but missing
  create_toy_world()
  my_model <- list(model_two = list(yes = 0.1, no = 0.9))
  world$add(x = list(yes = 0.5, no = 0.5), "model_two")
  REQUIRED_MODELS <- c("model_one", "model_two")
  expect_error(
    pick_models(my_model, world, REQUIRED_MODELS),
    "These required models are not present: model_one"
  )
})

test_that("pick_models - 3", {
  # find both models
  create_toy_world()
  my_model <- list(
    model_one = list(yes = 0.1, no = 0.9),
    model_two = list(yes = 0.1, no = 0.9)
  )
  world$add(x = list(yes = 0.5, no = 0.5), "model_one")
  world$add(x = list(yes = 0.5, no = 0.5), "model_two")
  REQUIRED_MODELS <- c("model_one", "model_two")
  res <- pick_models(my_model, world, REQUIRED_MODELS)
  expect_true(res$model_one$yes == 0.1)
  expect_true(res$model_two$yes == 0.1)
})

test_that("pick_models - 4", {
  # find both models
  my_model <- list(
    model_one = list(yes = 0.1, no = 0.9),
    model_two = list(yes = 0.1, no = 0.9)
  )
  REQUIRED_MODELS <- c("model_one", "model_two")
  pick_models(my_model, world, REQUIRED_MODELS)
})

test_that("pick_models - default behavior", {
  # event function set model to NULL as default
  create_toy_world()
  my_model <- NULL
  REQUIRED_MODELS <- c("model_one", "model_two")
  expect_error(
    pick_models(my_model, world, REQUIRED_MODELS),
    "These required models are not present: model_one, model_two"
  )
})

test_that("pick_models - default behavior 2", {
  # event function set model to NULL as default
  create_toy_world()
  my_model <- NULL
  world$add(x = list(yes = 0.5, no = 0.5), "model_one")
  REQUIRED_MODELS <- c("model_one", "model_two")
  expect_error(
    pick_models(my_model, world, REQUIRED_MODELS),
    "These required models are not present: model_two"
  )
})

test_that("pick_models - default behavior 3", {
  # event function set model to NULL as default
  create_toy_world()
  my_model <- NULL
  world$add(x = list(yes = 0.5, no = 0.5), "model_one")
  world$add(x = list(yes = 0.5, no = 0.5), "model_two")
  REQUIRED_MODELS <- c("model_one", "model_two")
  checkmate::expect_list(pick_models(my_model, world, REQUIRED_MODELS), names = "strict")
})

test_that("pick_models - provide an Model object", {
  # event function set model to NULL as default
  create_toy_world()
  my_model <- list(
    model_one = Model$new(list(yes = 0.5, no = 0.5)),
    model_two = Model$new(list(yes = 0.5, no = 0.5))
  )
  REQUIRED_MODELS <- c("model_one")
  picked_models <- pick_models(my_model, world, REQUIRED_MODELS)
  checkmate::expect_list(picked_models, names = "strict", len = 1)
  expect_true(names(picked_models) == REQUIRED_MODELS)
})


test_that("pick_models - deterministic case", {
  # event function set model to NULL as default
  create_toy_world()
  my_model <- NULL
  world$add(x = list(yes = 0.5, no = 0.5), "model_one")
  world$add(x = list(yes = 0.5, no = 0.5), "model_two")
  REQUIRED_MODELS <- c()
  expect_null(pick_models(my_model, world, REQUIRED_MODELS))
})

test_that("pick_models with as_r6model is TRUE", {
  create_toy_world()
  my_model <- NULL
  world$add(x = list(yes = 0.5, no = 0.5), "model_one")
  world$add(x = list(yes = 0.5, no = 0.5), "model_two")
  REQUIRED_MODELS <- c("model_one")
  models <- pick_models(my_model, world, REQUIRED_MODELS, as_r6model = TRUE)
  checkmate::expect_r6(models$model_one, classes = "Model")
})

test_that("pick_models with as_r6model is FALSE", {
  create_toy_world()
  my_model <- NULL
  world$add(x = list(yes = 0.5, no = 0.5), "model_one")
  world$add(x = list(yes = 0.5, no = 0.5), "model_two")
  REQUIRED_MODELS <- c("model_one")
  models <- pick_models(my_model, world, REQUIRED_MODELS, as_r6model = FALSE)
  checkmate::expect_list(models$model_one)
})
