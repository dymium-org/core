create_mlr_task <- function() {
  task_data <-
    dymiumCore::toy_individuals[, sex := as.factor(sex)][, marital_status := as.factor(marital_status)] %>%
    .[, .(age, sex, marital_status)] %>%
    as.data.frame()
}

create_mlr_binary_model <- function() {
  task_data <- create_mlr_task()
  task <- mlr::makeClassifTask(id = "toy_two_classes", data = task_data, target = "sex")
  lrn <- mlr::makeLearner("classif.glmboost", predict.type = "prob")
  train_mod <- mlr::train(lrn, task)
}

create_mlr_multinomial_model <- function() {
  task_data <- create_mlr_task()
  task <- mlr::makeClassifTask(id = "toy_multi_classes", data = task_data, target = "marital_status")
  lrn = mlr::makeLearner("classif.multinom", predict.type = "prob")
  train_mod <- mlr::train(lrn, task)
}

create_caret_binary_model <- function() {
  caret::train(
    as.factor(sex) ~ age + marital_status,
    data = toy_individuals,
    method = "glm",
    family = "binomial"
  )
}

create_caret_multinomial_model <- function() {
  caret::train(
    marital_status ~ age + sex,
    data = toy_individuals,
    method = "multinom",
    trace = FALSE
  )
}

create_glm_binary_model <- function() {
  stats::glm(
    as.factor(sex) ~ age + marital_status,
    data = toy_individuals,
    family = "binomial"
  )
}
