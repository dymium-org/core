test_that("initialize", {
  create_toy_population()
  Ind <- pop$get("Individual")
  Hh <- pop$get("Household")
  model <- list(yes = 1)
  expect_error(Trans$new(1, NULL), "Must inherit from class 'Agent', but has class 'numeric'")
  Trans <- checkmate::expect_class(Trans$new(Ind, model), "Trans")
  checkmate::expect_class(Trans, "Trans")
  checkmate::expect_class(Trans$get_result(), "data.table")
})

test_that("get_result", {
  create_toy_population()
  Ind <- pop$get("Individual")
  Hh <- pop$get("Household")
  model <- list(yes = 1)
  expect_error(Trans$new(1, NULL), "Must inherit from class 'Agent', but has class 'numeric'")
  Trans <- checkmate::expect_class(Trans$new(Ind, model), "Trans")
  expect_error(Trans$get_result(ids = c(1:10, 10, 9999)), regexp = 'Must be a subset of')
  checkmate::expect_data_table(Trans$get_result(ids = 1:10), any.missing = FALSE, nrows = 10, ncols = 2, col.names = 'strict', null.ok = FALSE)
  checkmate::expect_data_table(Trans$get_result(ids = c(1:10, 10)), any.missing = FALSE, nrows = 11, ncols = 2, col.names = 'strict', null.ok = FALSE)
  checkmate::expect_subset(Trans$get_result(ids = 1:10)[['id']], choices = c(1:10))
  checkmate::expect_set_equal(x = Trans$get_result(ids = c(1:8,10,9))[['id']], y = c(1:8,10,9), ordered = T)
  checkmate::expect_class(Trans, "Trans")
  checkmate::expect_class(Trans$get_result(), "data.table")
})

test_that("initialise by targeted_agents", {
  create_toy_population()
  Ind <- pop$get("Individual")
  Hh <- pop$get("Household")
  model <- list(yes = 1)
  idx <- 1:10
  ids <- Ind$get_data()[[Ind$get_id_col()]][idx]
  Trans <- Trans$new(Ind, model = model, targeted_agents = ids)
  expect_equal(Trans$get_result()[["id"]], ids)
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
      },
      mutate_first = TRUE
    )
  )

  ValidMutateFirstTrans <- R6::R6Class(
    classname = "ValidMutateFirstTrans",
    inherit = TransitionClassification,
    public = list(
      filter = function(.data) {
        .data %>%
          .[age < 100, ]
      },
      mutate = function(.data) {
        .data %>%
          .[, test_col := 100]
      },
      mutate_first = TRUE
    )
  )

  ValidNotMutateFirstTrans <- R6::R6Class(
    classname = "ValidNotMutateFirstTrans",
    inherit = TransitionClassification,
    public = list(
      filter = function(.data) {
        .data %>%
          .[age < 100, ]
      },
      mutate = function(.data) {
        .data %>%
          .[, test_col := 100]
      },
      mutate_first = FALSE
    )
  )

  Trans <- ValidMutateFirstTrans$new(Ind, model = model_ls)
  checkmate::expect_integerish(Trans$get_data()[["test_col"]], null.ok = FALSE, lower = 100, upper = 100)
  checkmate::expect_names(names(Trans$get_result()), identical.to = c("id", "response"))
  checkmate::expect_data_table(x = Trans$get_result(), ncols = 2, min.rows = 1)
  checkmate::expect_data_table(Trans$get_data())

  Trans <- ValidNotMutateFirstTrans$new(Ind, model = model_ls)
  checkmate::expect_integerish(Trans$get_data()[["test_col"]], null.ok = FALSE, lower = 100, upper = 100)
  checkmate::expect_names(names(Trans$get_result()), identical.to = c("id", "response"))
  checkmate::expect_data_table(x = Trans$get_result(), ncols = 2, min.rows = 1)
  checkmate::expect_data_table(Trans$get_data())

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

test_that("Transition works with a Model with a preprocessing function", {
  m <- Model$new(list(yes = 0.5, no = 0.5))

  m$preprocessing_fn <- function(.data) {
    .data %>%
      .[age %between% c(18, 40) &
          sex == "female"]
  }

  res <-
    TransitionClassification$new(world$entities$Individual, model = m)$get_result()
  ind_data <-
    world$entities$Individual$get_data(ids = res[["id"]])

  expect_true(all(ind_data[["sex"]] == "female"))
  expect_true(all(ind_data[["age"]] %between% c(18, 40)))
})


test_that("Transition is fair", {
  m <- Model$new(list(yes = 0.2, no = 0.8))
  responses <- c()
  for (i in 1:100) {
    responses <-
      c(
        responses,
        TransitionClassification$new(world$entities$Individual, model = m)$get_result()[, response]
      )
  }
  t_test_res <- t.test(x = responses == "yes", mu = 0.2)
  expect_gte(object = t_test_res$p.value, expected = 0.05)
})
