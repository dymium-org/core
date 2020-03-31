test_that("transition function works", {
  create_toy_world()

  # create a preprocessing filter function
  filter_male <-
    . %>%
    .[sex == "male",]

  transition(
    world,
    entity = "Individual",
    model = glm(factor(sex) ~ age, toy_individuals, family = binomial()),
    preprocessing_fn = filter_male
  )

  if (requireNamespace("caret")) {
    transition(
      world,
      entity = "Individual",
      model = caret::train(
        sex ~ age,
        data = toy_individuals,
        method = "glm",
        family = binomial()
      ),
      targeted_ids = 99,
      preprocessing_fn = filter_male
    )
  }

  # mnl
  mnl <- caret::train(
    marital_status ~ age + sex,
    data = toy_individuals,
    method = "multinom",
    trace = FALSE
  )

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
                                      preprocessing_fn = filter_male)
  checkmate::expect_data_table(
    x = transition_result,
    ncols = 2,
    any.missing = FALSE,
    null.ok = FALSE
  )

  checkmate::expect_names(names(transition_result),
                          permutation.of = c("id", "response"))
  checkmate::expect_integerish(
    transition_result[['id']],
    lower = 1,
    any.missing = FALSE,
    unique = TRUE,
    null.ok = FALSE
  )
  checkmate::expect_character(transition_result[['response']],
                              any.missing = FALSE,
                              null.ok = FALSE)

})


test_that("transition works with mlr models", {
  create_toy_world()
  my_model <- Model$new(x = create_mlr_multinomial_model())
  res <- get_transition(world, entity = "Individual", model = my_model)
  checkmate::expect_character(res[["response"]],
                              any.missing = FALSE,
                              null.ok = FALSE)
})
