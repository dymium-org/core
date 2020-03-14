test_that("Create Transition2", {
  create_toy_world()
  my_model <- list(yes = 0.6, no = 0.4)
  TransitionObj <- Transition2$new(Ent = world$entities$Individual, model = my_model)

  checkmate::expect_r6(TransitionObj$Ent, classes = "Entity")
  checkmate::expect_data_table(TransitionObj$Ent_data, min.rows = 1, min.cols = 1, null.ok = FALSE)

  # with preprocessing functions
  preprocessing_steps <-
    . %>%
    .[age < 10, ] %>%
    .[sex == "male", ]

  TransitionObj <-
    Transition2$new(Ent = world$entities$Individual,
                    model = my_model,
                    preProcess_fns = preprocessing_steps)

  expect_data_table(TransitionObj$Ent_data)
  expect_true(all(TransitionObj$Ent_data[['sex']] == "male"))
  expect_true(all(TransitionObj$Ent_data[['age']] < 10))
})
