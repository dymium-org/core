test_that("train", {

  create_toy_population()
  Ind <- pop$get("Individual")
  Hh <- pop$get("Household")

  # create model
  fitting_data <- Ind$get_data()[, male := ifelse(sex == 'male', 'yes', 'no')]
  model <- create_caret_binary_model()

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


test_that("datatable - dynamic rate", {
  create_toy_population()
  Ind <- pop$get("Individual")

  # create an enumerated dynamic rate model - age, sex, t_*
  model <- data.table(CJ(sex = c('male', 'female'), age = 0:100)) %>%
    .[, paste0("t_", c(0:10)) := runif(.N)]

  model_bad1 <- data.table(CJ(sex = c('male', 'female'), age = 0:100)) %>%
    .[, paste0("t_", LETTERS) := runif(.N)]

  model_bad2 <- data.table(CJ(sex = c('male', 'female'), age = 0:100)) %>%
    .[, paste0("t_", LETTERS, seq_along(LETTERS)) := runif(.N)]

  model_bad3 <- data.table(CJ(sex = c('male', 'female'), age = 0:100)) %>%
    .[, paste0("t_", c(0:10), LETTERS[1:10]) := runif(.N)]

  # test models
  expect_true(is_dynamic_rate_datatable_model(model, Ind$get_data()))
  expect_false(is_dynamic_rate_datatable_model(model_bad1, Ind$get_data()))
  expect_false(is_dynamic_rate_datatable_model(model_bad2, Ind$get_data()))
  expect_false(is_dynamic_rate_datatable_model(model_bad3, Ind$get_data()))

  a_transition <- TransitionClassification$new(Ind, model)
  checkmate::expect_r6(a_transition, "TransitionClassification")
  checkmate::expect_data_table(a_transition$get_result(), ncols = 2)
  checkmate::expect_subset(a_transition$get_result()[['response']], c("yes", "no"))
  checkmate::expect_integerish(a_transition$get_result()[['id']], lower = 1, unique = T, null.ok = FALSE, any.missing = FALSE)
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

  # some prob doesn't sum up to 1
  bad_choice3 <- data.table(
    sex = c('male', 'female'),
    probs = list(c(0.1,0.8), c(0.9,0.1)),
    choices = list(c('can drive', 'cannot drive'), c('can drive', 'cannot drive'))
  )

  # duplicated choice
  bad_choice <- data.table(
    sex = c('male', 'female', 'male'),
    probs = list(c(0.1,0.9), c(0.9,0.1), c(0.9,0.1)),
    choices = list(c('can drive', 'cannot drive'), c('can drive', 'cannot drive'), c('can drive', 'cannot drive'))
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

  checkmate::expect_data_table(TransitionCandrive$new(Ind, good_choice2)$get_result())
  expect_message(print(TransitionCandrive$new(Ind, good_choice)),
                 regexp = "agents with 2 unique responses of type character")
  expect_message(print(TransitionCandrive$new(Ind, good_choice2)),
                 regexp = "unique responses of type character")
  expect_error(TransitionCandrive$new(Ind, bad_choice),
               regexp = "`model` contains duplicated rows")
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
  expect_equal(nrow(a_good_transition_2$get_result()), nrow(Ind$get_data()))
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


test_that("model by targeted_agents", {
  create_toy_population()
  Ind <- pop$get("Individual")
  Hh <- pop$get("Household")
  idx <- 1:3
  ids <- Ind$get_data()[[Ind$get_id_col()]][idx]

  my_model <- data.table(
    pid = c(1:3),
    probs = list(c(0.5,0.2,0.3), c(0.5,0.2,0.3), c(0.5,0.2,0.3)),
    choices = list(sample(letters, 3, replace = T), sample(letters, 3, replace = T), sample(letters, 3, replace = T))
  )

  a_transition <- TransitionClassification$new(Ind, model = my_model, targeted_agents = ids, model_by_id = TRUE)

  expect_equal(a_transition$get_result()[["id"]], ids)
})

test_that("update", {
  create_toy_population()
  Ind <- pop$get("Individual")
  ids <- sample(Ind$get_ids(), 10, replace = FALSE)

  # create model
  vector_model <- c(choice_a = 0.2, choice_b = 0.8)

  # create transition
  TransVec <- TransitionClassification$new(Ind, vector_model, targeted_agents = ids)
  TransVec$update_agents(attr = "test")
  expect_true("test" %in% names(Ind$get_data()))
  checkmate::assert_character(names(table(Ind$get_attr("test"))), min.len = 1, unique = TRUE, null.ok = FALSE)
})


test_that("dynamic target", {

  create_toy_world()

  model <- list(yes = 0.10, no = 0.90)

  dynamic_target <-
    data.table(
      time = c(1:10),
      yes = sample(1:20, 10, replace = TRUE),
      no = sample(1:20, 10, replace = TRUE)
    )

  TargetDynamic <-
    data.table::data.table(
      time = c(1:10),
      yes = sample(1:20, 10, replace = TRUE),
      no = sample(1:20, 10, replace = TRUE)
    ) %>%
    Target$new(.)

  event_dynamic_target <- function(world, model, target) {

    Ind <- world$get("Individual")

    DynamicTrans <- TransitionClassification$new(Ind, model, target)

    remove_ids <- DynamicTrans$get_result()[response == "yes", id]

    if (length(remove_ids) > 0) {
      Ind$remove(ids = remove_ids)
    }

    return(world)
  }

  Ind <- world$get("Individual")

  n_ind_before <- Ind$n()

  for (i in 1:10) {
    world$start_iter(time_step = i, unit = "year") %>%
      event_dynamic_target(., model, target = dynamic_target)
  }

  n_ind_after <- Ind$n()

  expect_true(n_ind_after + sum(dynamic_target$yes) == n_ind_before)

  # bad target, `nooo` is not a valid response
  dynamic_target <-
    data.table(
      time = c(1:10),
      yes = sample(1:20, 10, replace = TRUE),
      no = sample(1:20, 10, replace = TRUE),
      nooo = sample(1:20, 10, replace = TRUE)
    )

  expect_error(TransitionClassification$new(world$entities$Individual, model, dynamic_target),
               regexp = "Must be a subset of set \\{yes,no\\}.")
})

test_that("TransitionClassifcation works with mlr model", {
  if (requireNamespace("mlr") &
      requireNamespace("nnet")) {
    create_toy_world()
    mlr_model <- create_mlr_multinomial_model()
    my_trans <-
      TransitionClassification$new(world$entities$Individual, model = mlr_model)
    checkmate::expect_character(
      my_trans$get_result()[["response"]],
      any.missing = FALSE,
      len = world$entities$Individual$n()
    )
  }
})


test_that("TransitionClassifition$draw", {
  create_toy_population()
  Ind <- pop$get("Individual")
  Hh <- pop$get("Household")

  # create model
  fitting_data <- Ind$get_data()[, male := ifelse(sex == 'male', 'yes', 'no')]
  caret_binary_model <- create_caret_binary_model()
  glm_binary_model <- create_glm_binary_model()

  # create transition
  a_transition <- TransitionClassification$new(Ind, model)
  res1 <- a_transition$draw(target = list(male = 10))
  res2 <- a_transition$draw(target = Target$new(list(male = 10, female = 10)))

  checkmate::expect_integerish(res1[['id']])
  checkmate::expect_integerish(res2[['id']])

  checkmate::expect_character(res1[['response']], pattern = "^male$|^female$", any.missing = FALSE)
  checkmate::expect_character(res2[['response']], pattern = "^male$|^female$", any.missing = FALSE)
})
