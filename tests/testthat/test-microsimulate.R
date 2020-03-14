test_that("microsimulation.glm", {
  glm_mod <-
    stats::glm(as.factor(sex) ~ age + marital_status,
        toy_individuals,
        family = binomial("logit"))

  train_mod <-
    caret::train(
      as.factor(sex) ~ age + marital_status,
      data = toy_individuals,
      method = "glm",
      family = binomial("logit"))


  microsimulate(glm_mod, newdata = toy_individuals)
  microsimulate(train_mod, toy_individuals)
})
