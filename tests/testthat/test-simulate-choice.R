test_that("simulate_choice.glm works", {
  glm_mod <- create_glm_binary_model()
  checkmate::expect_character(simulate_choice(glm_mod, newdata = toy_individuals),
    pattern = "male|female",
    any.missing = FALSE,
    len = nrow(toy_individuals)
  )
})


test_that("simulate_choice.caret works", {
  train_mod <- create_caret_binary_model()
  checkmate::expect_character(simulate_choice(train_mod, newdata = toy_individuals),
    pattern = "male|female",
    any.missing = FALSE,
    len = nrow(toy_individuals)
  )
})


test_that("simulate_choice.train multilabels works", {
  train_mod <- create_caret_multinomial_model()
  checkmate::expect_character(simulate_choice(train_mod, newdata = toy_individuals),
    pattern = paste(unique(toy_individuals[["marital_status"]]), collapse = "|"),
    any.missing = FALSE,
    len = nrow(toy_individuals)
  )
})


test_that("simulate_choice.data.frame works", {
  n_rows <- 10
  probs <- data.frame(yes = runif(n_rows), no = runif(n_rows), maybe = runif(n_rows))
  checkmate::expect_character(simulate_choice(probs),
    pattern = "yes|no|maybe",
    any.missing = FALSE,
    len = n_rows
  )
})

test_that("simulate_choice.WrappedModel from mlr works", {

  # two classes
  if (requireNamespace("mlr")) {
    train_mod <- create_mlr_binary_model()
    checkmate::expect_character(simulate_choice(train_mod, newdata = toy_individuals),
      pattern = "male|female",
      any.missing = FALSE,
      len = nrow(toy_individuals)
    )
  }

  # multi classes
  if (requireNamespace("mlr") & requireNamespace("nnet")) {
    train_mod <- create_mlr_multinomial_model()
    checkmate::expect_character(simulate_choice(train_mod, newdata = toy_individuals),
      pattern = paste(unique(toy_individuals[["marital_status"]]), collapse = "|"),
      any.missing = FALSE,
      len = nrow(toy_individuals)
    )
  }
})

test_that("simulate_choice.Model from dymiumCore works", {
  if (requireNamespace("mlr") & requireNamespace("nnet")) {
    train_mod <- create_mlr_multinomial_model()
    my_model <- Model$new(train_mod, preprocessing_fn = . %>% .[sex == "male"])
    sim_res <- simulate_choice(my_model, newdata = toy_individuals)
    checkmate::expect_character(simulate_choice(my_model, newdata = toy_individuals),
      pattern = paste(unique(toy_individuals[["marital_status"]]), collapse = "|"),
      any.missing = FALSE,
      len = toy_individuals[sex == "male", .N]
    )
  }
})
