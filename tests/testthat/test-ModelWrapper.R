test_that("ModelWrapper works", {
  mod = ModelWrapper$new(list(x = 1), name = "ModelWrapper")
  checkmate::expect_r6(mod, classes = "ModelWrapper")

})


test_that("ModelTidymodel works", {

  # tidymodel classification ----------------------------------------------------------
  logistic_reg =
    parsnip::logistic_reg() %>%
    parsnip::set_engine("glm") %>%
    parsnip::fit(I(as.factor(sex)) ~ age + marital_status, data = toy_individuals)

  predict(logistic_reg, new_data = toy_individuals, type = "prob")

  random_forest_classif =
    parsnip::rand_forest() %>%
    parsnip::set_engine("ranger") %>%
    parsnip::set_mode("classification") %>%
    parsnip::fit(I(as.factor(sex)) ~ age + marital_status, data = toy_individuals)

  predict(random_forest_classif, new_data = toy_individuals, type = "prob")

  multinom_reg =
    parsnip::multinom_reg() %>%
    parsnip::set_engine("glmnet") %>%
    parsnip::set_mode("classification") %>%
    parsnip::fit(I(as.factor(marital_status)) ~ age + sex, data = toy_individuals)

  predict(multinom_reg, new_data = toy_individuals, type = "prob", penalty = 0)

  # tidymodel regression ----------------------------------------------------------
  linear_reg =
    parsnip::linear_reg() %>%
    parsnip::set_engine("lm") %>%
    parsnip::fit(age ~ sex + marital_status, data = toy_individuals)

  predict(linear_reg, new_data = toy_individuals)

  random_forest_reg =
    parsnip::rand_forest() %>%
    parsnip::set_engine("ranger") %>%
    parsnip::set_mode("regression") %>%
    parsnip::fit(age ~ sex + marital_status, data = toy_individuals)

  predict(random_forest_reg, new_data = toy_individuals)

})

test_that("ModelGlm works", {

  # glm classification ------------------------------------------------------
  logistic_reg =
    glm(I(as.factor(sex)) ~ age + marital_status, data = toy_individuals, family = "binomial")

  # lm regression ------------------------------------------------------
  linear_reg =
    glm(age ~ sex + marital_status, data = toy_individuals)
})

test_that("ModelCaret works", {

})

test_that("ModelMlr3 works", {

})

test_that("ModelMlr works", {

})

test_that("ModelMlogit works", {

})
