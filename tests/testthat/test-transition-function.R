test_that("transition function works", {
  create_toy_world()

  # create a preprocessing filter function
  filter_male <-
    . %>%
    .[sex == "male", ]

  transition(
    world,
    entity = "Individual",
    model = create_glm_binary_model(),
    preprocessing_fn = filter_male
  )

  if (requireNamespace("caret")) {
    transition(
      world,
      entity = "Individual",
      model = create_caret_binary_model(),
      targeted_ids = 99,
      preprocessing_fn = filter_male
    )
  }

  # mnl
  mnl <- create_caret_multinomial_model()

  checkmate::expect_r6(
    x = transition(
      world,
      entity = "Individual",
      model = mnl,
      preprocessing_fn = filter_male
    ),
    classes = "World"
  )

  transition_result <- get_transition(world,
    entity = "Individual",
    model = mnl,
    preprocessing_fn = filter_male
  )
  checkmate::expect_data_table(
    x = transition_result,
    ncols = 2,
    any.missing = FALSE,
    null.ok = FALSE
  )

  checkmate::expect_names(names(transition_result),
    permutation.of = c("id", "response")
  )
  checkmate::expect_integerish(
    transition_result[["id"]],
    lower = 1,
    any.missing = FALSE,
    unique = TRUE,
    null.ok = FALSE
  )
  checkmate::expect_character(transition_result[["response"]],
    any.missing = FALSE,
    null.ok = FALSE
  )
})


test_that("transition works with mlr models", {
  if (requireNamespace("mlr") & requireNamespace("nnet")) {
    create_toy_world()
    my_model <- Model$new(x = create_mlr_multinomial_model())
    res <-
      get_transition(world, entity = "Individual", model = my_model)
    checkmate::expect_character(res[["response"]],
      any.missing = FALSE,
      null.ok = FALSE
    )
  }
})


test_that("transition's values param works", {
  birth_model <- list(yes = 0.1, no = 0.9)
  death_model <- list(yes = 0.5, no = 0.5)

  ind_data <-
    data.table::copy(toy_individuals) %>%
    .[, give_birth := "no"]

  world <- World$new()
  world$add(x = Individual$new(.data = ind_data, id_col = "pid"))

  world %>%
    transition(
      entity = "Individual",
      model = birth_model,
      attr = "give_birth"
    ) %>%
    transition(
      entity = "Individual",
      model = death_model,
      attr = "age",
      values = c(yes = -1L)
    )

  checkmate::expect_data_table(
    x = world$entities$Individual$get_data(),
    nrows = nrow(ind_data),
    col.names = "strict"
  )
  checkmate::expect_names(names(world$entities$Individual$get_data()),
    permutation.of = names(ind_data)
  )
  checkmate::expect_integerish(world$entities$Individual$get_data()[, age],
    lower = -1,
    any.missing = FALSE
  )
  checkmate::expect_character(world$entities$Individual$get_data()[, give_birth],
    pattern = "yes|no"
  )
})

test_that("transition's works with Model object", {
  create_toy_world()
  my_model <- Model$new(
    x = create_mlr_multinomial_model(),
    preprocessing_fn = . %>% .[sex == "male"]
  )
  res <- get_transition(world, entity = "Individual", model = my_model)
  expect_true(all(world$entities$Individual$get_attr("sex", ids = res[["id"]]) == "male"))
  checkmate::expect_character(res[["response"]], any.missing = FALSE, null.ok = FALSE)
})

test_that("remove dot prefix of derived variables", {
  create_toy_world()
  world$entities$Individual$get_data(copy = FALSE) %>%
    .[, .married_male := fifelse(marital_status == "married" & sex == "male", T, F)]
  fit_data <- normalise_derived_vars(world$entities$Individual$get_data())
  logit_model <- glm(factor(sex) ~ married_male, data = fit_data, family = "binomial")
  my_model <- Model$new(
    x = logit_model,
    preprocessing_fn = . %>% .[sex == "male"]
  )
  res <- get_transition(world, entity = "Individual", model = my_model)
  expect_true(all(world$entities$Individual$get_attr("sex", ids = res[["id"]]) == "male"))
  checkmate::expect_character(res[["response"]], any.missing = FALSE, null.ok = FALSE)
})
