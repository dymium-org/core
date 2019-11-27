test_that("train", {

  create_toy_population()
  Ind <- pop$get("Individual")
  Hh <- pop$get("Household")

  # create model
  fitting_data <- Ind$get_data()[, male := ifelse(sex == 'male', 'yes', 'no')]
  model <- caret::train(male ~ age + marital_status, data = fitting_data, method = 'glm', family = binomial('logit'))

  # create transition
  a_transition <- TransitionClassification$new(Ind, model)

  # validate the result
  checkmate::expect_data_table(a_transition$get_result(), any.missing = FALSE, min.cols = 1, ncols = 2, null.ok = FALSE)
  checkmate::expect_names(names(a_transition$get_result()), permutation.of = c('id', 'response'))

})

test_that("datatable - binary", {

  create_toy_population()
  Ind <- pop$get("Individual")

  # create an enumerated binary model - age, sex, prob
  model <- data.table(CJ(sex = c('male', 'female'), age = 0:100))[, prob := runif(.N)]

  # create transition
  a_transition <- TransitionClassification$new(Ind, model)

  # validate the result
  checkmate::expect_data_table(a_transition$get_result(), any.missing = FALSE, min.cols = 1, ncols = 2, null.ok = FALSE)
  checkmate::expect_names(names(a_transition$get_result()), permutation.of = c('id', 'response'))
})


test_that("datatable - choices", {

  create_toy_population()

  Ind <- pop$get("Individual")

  good_choice <- data.table(
    sex = c('male', 'female'),
    probs = list(c(0.1,0.9), c(0.9,0.1)),
    choices = list(c('can drive', 'cannot drive'), c('can drive', 'cannot drive'))
  )

  good_choice2 <- data.table(
    sex = c('male', 'female'),
    probs = list(c(0.1,0.9), c(0.8,0.1,0.1)),
    choices = list(c('can drive', 'cannot drive'), c('can drive', 'cannot drive', 'not applicable'))
  )

  # duplicated choice
  bad_choice <- data.table(
    sex = c('male', 'female', 'male'),
    probs = list(c(0.1,0.9), c(0.9,0.1), c(0.9,0.1)),
    choices = list(c('can drive', 'cannot drive'), c('can drive', 'cannot drive'), c('can drive', 'cannot drive'))
  )

  # some prob doesn't sum up to 1
  bad_choice2 <- data.table(
    sex = c('male', 'female'),
    probs = list(c(0.1,0.8), c(0.9,0.1)),
    choices = list(c('can drive', 'cannot drive'), c('can drive', 'cannot drive'))
  )

  # contain an extra column
  bad_choice3 <- data.table(
    sex = c('male', 'female'),
    dummy = c('a', 'b'),
    probs = list(c(0.1,0.9), c(0.9,0.1)),
    choices = list(c('can drive', 'cannot drive'), c('can drive', 'cannot drive'))
  )

  TransitionCandrive <- R6::R6Class(classname = "TransitionCandrive",
                                    inherit = TransitionClassification)

  expect_message(print(TransitionCandrive$new(Ind, good_choice)),
                 regexp = "agents with 2 unique responses of type character")
  expect_message(print(TransitionCandrive$new(Ind, good_choice2)),
                 regexp = "unique responses of type character")
  expect_error(TransitionCandrive$new(Ind, bad_choice),
               regexp = "`model` contains duplicated rows")
  expect_error(TransitionCandrive$new(Ind, bad_choice2),
               regexp = "probability and choice columns failed to pass the sanity checks of")
  expect_error(TransitionCandrive$new(Ind, bad_choice3),
               regexp = "failed: Must be a subset of set \\{pid")
})

test_that("list and numeric", {

  create_toy_population()
  Ind <- pop$get("Individual")

  # create model
  list_model <- list(choice_a = 0.2, choice_b = 0.8)
  vector_model <- c(choice_a = 0.2, choice_b = 0.8)

  # create transition
  a_vector_transition <- TransitionClassification$new(Ind, vector_model)
  a_list_transition <- TransitionClassification$new(Ind, list_model)

  # validate the result
  checkmate::expect_data_table(a_vector_transition$get_result(), any.missing = FALSE, min.cols = 1, ncols = 2, null.ok = FALSE)
  checkmate::expect_names(names(a_vector_transition$get_result()), permutation.of = c('id', 'response'))

  checkmate::expect_data_table(a_list_transition$get_result(), any.missing = FALSE, min.cols = 1, ncols = 2, null.ok = FALSE)
  checkmate::expect_names(names(a_list_transition$get_result()), permutation.of = c('id', 'response'))

})


test_that('targeted_agent works', {

  create_toy_population()
  Ind <- pop$get("Individual")

  # create model
  list_model <- list(choice_a = 0.2, choice_b = 0.8)

  n_targeted_agents <- 10
  rand_ids <- sample(Ind$get_attr(Ind$get_id_col()), n_targeted_agents, replace = FALSE)

  # create transition
  a_transition <-
    TransitionClassification$new(Ind, list_model, targeted_agents = rand_ids)

  # validate the result
  checkmate::assert_integerish(
    x = a_transition$get_result()[['id']],
    lower = 1, len = n_targeted_agents, null.ok = FALSE, any.missing = FALSE, unique = TRUE)
  checkmate::expect_data_table(
    x = a_transition$get_result(),
    any.missing = FALSE, min.cols = 1, ncols = 2, null.ok = FALSE)

})

test_that('target works', {

  create_toy_population()
  Ind <- pop$get("Individual")

  # create model
  list_model <- list(choice_a = 0.2, choice_b = 0.8)

  # create targets
  good_target1 = list(choice_a = 10, choice_b = 20)
  good_target2 = list(choice_a = 10) # only choice a is controled
  bad_target1 = list(choice_a = 10, choice_b = 99999)
  bad_target2 = list(choice_a = 10, choice_b = -99999)
  bad_target3 = list(choice_a = 10, choice_c = 1) # no choice_c!

  # create transition
  a_good_transition_1 <-
    TransitionClassification$new(Ind, model = list_model, target = good_target1)
  expect_equal(as.numeric(table(a_good_transition_1$get_result()[['response']])),
               c(10, 20))
  a_good_transition_2 <-
    TransitionClassification$new(Ind, model = list_model, target = good_target2)
  expect_equal(as.numeric(table(a_good_transition_2$get_result()[['response']])),
               c(10))
  expect_error(
    TransitionClassification$new(Ind, model = list_model, target = bad_target1),
    "The sum of targets cannot exceed the number of agents that are undergoing this transition."
  )
  expect_error(
    TransitionClassification$new(Ind, model = list_model, target = bad_target2),
    "Element 2 is not >= 1."
  )
  expect_error(
    TransitionClassification$new(Ind, model = list_model, target = bad_target3),
    "Must be a subset of set \\{choice_a,choice_b\\}."
  )

})

