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
