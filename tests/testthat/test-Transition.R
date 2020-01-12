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
  model_r <- caret::train(age ~ sex + marital_status, data = Ind$get_data(), method = 'glm', family = gaussian())
  model_c <- caret::train(I(ifelse(sex == "male", "yes", "no")) ~ age + marital_status, data = Ind$get_data(), method = 'glm', family = binomial())
  model_lm <- glm(age ~ sex + marital_status, data = Ind$get_data(), family = "gaussian")
  model_glm <- glm(I(sex == "male") ~ age + marital_status, data = Ind$get_data(), family = "binomial")

  checkmate::expect_data_table(transition(Ind, model_r))
  checkmate::expect_data_table(transition(Ind, model_c))
  checkmate::expect_data_table(transition(Ind, model_lm))
  checkmate::expect_data_table(transition(Ind, model_glm))

})


# load data
create_toy_population()
Ind <- pop$get("Individual")

# create model
model_lm <- glm(age ~ sex + marital_status, data = Ind$get_data(), family = "gaussian")
model_glm <- glm(I(sex == "male") ~ age + marital_status, data = Ind$get_data(), family = "binomial")

# simulation transition
transition(Ind, model_lm)
transition(Ind, model_glm)
