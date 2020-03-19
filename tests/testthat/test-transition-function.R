test_that("transition function works", {
  create_toy_world()

  filter_male <-
    . %>%
    .[sex == "male",]

  transition(
    world,
    entity = "Individual",
    model = glm(factor(sex) ~ age, toy_individuals, family = binomial()),
    preprocess_fn = filter_male
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
      preprocess_fn = filter_male
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
      preprocess_fn = filter_male
    ),
    classes = "World"
  )

  transition_result <- get_transition(world,
                                      entity = "Individual",
                                      model = mnl,
                                      preprocess_fn = filter_male)
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
