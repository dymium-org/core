test_that("initialize", {
  create_toy_population()
  Ind <- pop$get("Individual")
  Hh <- pop$get("Household")
  model <- list(yes = 1)
  expect_error(Transition$new(1, NULL), "Must inherit from class 'Agent', but has class 'numeric'")
  Trans <- checkmate::expect_class(Transition$new(Ind, model), "Transition")
  checkmate::expect_class(Trans, "Transition")
  checkmate::expect_class(Trans$get_result(), "data.table")
})

test_that("get_result", {
  create_toy_population()
  Ind <- pop$get("Individual")
  Hh <- pop$get("Household")
  model <- list(yes = 1)
  expect_error(Transition$new(1, NULL), "Must inherit from class 'Agent', but has class 'numeric'")
  Trans <- checkmate::expect_class(Transition$new(Ind, model), "Transition")
  expect_error(Trans$get_result(ids = c(1:10, 10, 9999)), regexp = 'Must be a subset of')
  checkmate::expect_data_table(Trans$get_result(ids = 1:10), any.missing = FALSE, nrows = 10, ncols = 2, col.names = 'strict', null.ok = FALSE)
  checkmate::expect_data_table(Trans$get_result(ids = c(1:10, 10)), any.missing = FALSE, nrows = 11, ncols = 2, col.names = 'strict', null.ok = FALSE)
  checkmate::expect_subset(Trans$get_result(ids = 1:10)[['id']], choices = c(1:10))
  checkmate::expect_set_equal(x = Trans$get_result(ids = c(1:8,10,9))[['id']], y = c(1:8,10,9), ordered = T)
  checkmate::expect_class(Trans, "Transition")
  checkmate::expect_class(Trans$get_result(), "data.table")
})

test_that("initialise by targeted_agents", {
  create_toy_population()
  Ind <- pop$get("Individual")
  Hh <- pop$get("Household")
  model <- list(yes = 1)
  idx <- 1:10
  ids <- Ind$get_data()[[Ind$get_id_col()]][idx]
  Trans <- Transition$new(Ind, model = model, targeted_agents = ids)
  expect_equal(Trans$get_result()[["id"]], ids)
})


test_that("transition fn", {

  create_toy_population()
  Ind <- pop$get("Individual")

  # create model
  model_ls <- list(choice_a = 10, choice_b = 20)
  model_r <- caret::train(age ~ sex + marital_status, data = Ind$get_data(), method = 'glm', family = gaussian())
  model_c <- caret::train(I(ifelse(sex == "male", "yes", "no")) ~ age + marital_status, data = Ind$get_data(), method = 'glm', family = binomial())
  model_lm <- glm(age ~ sex + marital_status, data = Ind$get_data(), family = "gaussian")
  model_glm <- glm(I(sex == "male") ~ age + marital_status, data = Ind$get_data(), family = "binomial")

  checkmate::expect_data_table(trans(Ind, model_ls))
  checkmate::expect_data_table(trans(Ind, model_r))
  checkmate::expect_data_table(trans(Ind, model_c))
  checkmate::expect_data_table(trans(Ind, model_lm))
  checkmate::expect_data_table(trans(Ind, model_glm))

})

test_that("transition 0 agents", {
  create_toy_population()
  Ind <- pop$get("Individual")

  # create model
  model_ls <- list(choice_a = 10, choice_b = 20)

  TestTransition <- R6::R6Class(
    classname = "TestTransition",
    inherit = TransitionClassification,
    public = list(
      filter = function(.data) {
        .data %>%
          .[age > 100, ]
      }
    )
  )

  TestTransitionMutateFirst <- R6::R6Class(
    classname = "TestTransitionMutateFirst",
    inherit = TransitionClassification,
    public = list(
      filter = function(.data) {
        .data %>%
          .[age > 100, ]
      },
      mutate = function(.data) {
        .data %>%
          .[, test_col := 100]
      }
    )
  )

  TestTran <- TestTransition$new(Ind, model = model_ls)
  checkmate::expect_names(names(TestTran$get_result()), identical.to = c("id", "response"))
  checkmate::expect_data_table(x = TestTran$get_result(), ncols = 2, nrows = 0)
  expect_null(TestTran$get_data())

  TestMutateFirstTran <- TestTransitionMutateFirst$new(Ind, model = model_ls)
  checkmate::expect_names(names(TestMutateFirstTran$get_result()), identical.to = c("id", "response"))
  checkmate::expect_data_table(x = TestMutateFirstTran$get_result(), ncols = 2, nrows = 0)
  expect_null(TestMutateFirstTran$get_data())

  TestMutateFirstTranTargetedAgents <-
    TestTransitionMutateFirst$new(Ind, model = model_ls, targeted_agents = 1)
  checkmate::expect_names(names(TestMutateFirstTranTargetedAgents$get_result()), identical.to = c("id", "response"))
  checkmate::expect_data_table(x = TestMutateFirstTranTargetedAgents$get_result(), ncols = 2, nrows = 0)
  expect_null(TestMutateFirstTranTargetedAgents$get_data())

  TestMutateFirstTranEmptiedTargetedAgents <-
    TestTransitionMutateFirst$new(Ind, model = model_ls, targeted_agents = integer())
  checkmate::expect_names(names(TestMutateFirstTranEmptiedTargetedAgents$get_result()), identical.to = c("id", "response"))
  checkmate::expect_data_table(x = TestMutateFirstTranEmptiedTargetedAgents$get_result(), ncols = 2, nrows = 0)
  expect_null(TestMutateFirstTranEmptiedTargetedAgents$get_data())

})
