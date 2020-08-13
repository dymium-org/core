test_that("Model initialisation", {
  m <- Model$new(list(x = 1), name = "model")
  expect_true(m$name == "model")

  m <- Model$new(list(x = 1))
  expect_null(m$null)
})

test_that("Model - list", {
  m <- Model$new(list(x = 1))
  expect_is(m$get(), "list")
  expect_error(Model$new(TRUE), "not one of the supported models in Transition")
})

test_that("Model - glm", {
  m <- Model$new(glm(Species ~ ., data = iris, family = binomial()))
  checkmate::expect_class(m$model, "glm")
})

test_that("Model - train", {
  if (requireNamespace("caret")) {
    m <-
      Model$new(caret::train(
        Species ~ .,
        data = iris,
        trace = FALSE,
        trControl = caret::trainControl(method = "none")
      ))
    checkmate::expect_class(m$model, "train")
  }
})


test_that("Model - preprocess", {
  m <- Model$new(list(x = 1))

  m$preprocessing_fn <- function(.data) {
    .data %>%
      .[age %between% c(18, 40) &
          sex == "female"]
  }

  checkmate::expect_function(m$preprocessing_fn)
  checkmate::expect_data_table(m$preprocessing_fn(toy_individuals))
  expect_true(all(m$preprocessing_fn(toy_individuals)[["sex"]] == "female"))
  expect_true(all(m$preprocessing_fn(toy_individuals)[["age"]] %between% c(18, 40)))
})

test_that("Model works with mlr model object", {
  if (requireNamespace("mlr") & requireNamespace("nnet")) {
    task_data <-
      dymiumCore::toy_individuals[, sex := as.factor(sex)][, marital_status := as.factor(marital_status)] %>%
      .[, .(age, sex, marital_status)] %>%
      as.data.frame()
    task <- mlr::makeClassifTask(id = "toy_multi_classes", data = task_data, target = "marital_status")
    lrn = mlr::makeLearner("classif.multinom", predict.type = "prob")
    train_mod <- mlr::train(lrn, task)
    my_model <- Model$new(train_mod)
    checkmate::expect_r6(my_model, classes = "Model")
    checkmate::expect_class(my_model$model, classes = "WrappedModel")
    expect_equal(summary(my_model),
                 summary(my_model$model$learner.model))
  }
})
