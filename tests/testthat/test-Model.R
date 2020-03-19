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


