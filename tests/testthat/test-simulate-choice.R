test_that("simulate_choice.glm works", {
  glm_mod <-
    stats::glm(as.factor(sex) ~ age + marital_status,
        data = toy_individuals,
        family = binomial("logit"))
  checkmate::expect_character(simulate_choice(glm_mod, newdata = toy_individuals),
                              pattern = "male|female",
                              any.missing = FALSE,
                              len = nrow(toy_individuals))
})


test_that("simulate_choice.caret works", {
  train_mod <-
    caret::train(
      as.factor(sex) ~ age + marital_status,
      data = toy_individuals,
      method = "glm",
      family = binomial("logit"))
  checkmate::expect_character(simulate_choice(train_mod, newdata = toy_individuals),
                              pattern = "male|female",
                              any.missing = FALSE,
                              len = nrow(toy_individuals))
})


test_that("simulate_choice.train multilabels works", {
  train_mod <-
    caret::train(
      as.factor(marital_status) ~ age + sex,
      data = toy_individuals,
      method = "multinom",
      family = binomial("logit"),
      trace = FALSE)
  checkmate::expect_character(simulate_choice(train_mod, newdata = toy_individuals),
                              pattern = paste(unique(toy_individuals[["marital_status"]]), collapse = "|"),
                              any.missing = FALSE,
                              len = nrow(toy_individuals))
})


test_that("simulate_choice.data.frame works", {
  n_rows <- 10
  probs <- data.frame(yes = runif(n_rows), no = runif(n_rows), maybe = runif(n_rows))
  checkmate::expect_character(simulate_choice(probs),
                              pattern = "yes|no|maybe",
                              any.missing = FALSE,
                              len = n_rows)
})
