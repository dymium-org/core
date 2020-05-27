test_that("ModelCustom - linear regression and binary logit", {

  num_rows <- 100

  test_data <- data.frame(
    id = 1:num_rows,
    sex = sample(c("male", "female"), size = num_rows, replace = T),
    age = sample(1:100, num_rows, replace = T),
    x1 = runif(num_rows),
    x2 = runif(num_rows)
  )

  my_formula <- ~ x1 + x2 + I(x1^2) + x1:x2
  params = c(`(Intercept)` = 1, x1 = 2.5, x2 = -3, `I(x1^2)` = 0.5 , `x1:x2` = 1)

  m <- ModelCustom$new(params = params, formula = my_formula, type = "regression")
  checkmate::expect_numeric(m$predict(test_data), finite = T, any.missing = FALSE, len = num_rows)

  m <- ModelCustom$new(params = params, formula = my_formula, type = "binary")
  checkmate::expect_numeric(m$predict(test_data),lower = 0, upper = 1, finite = T, any.missing = FALSE, len = num_rows)

})

test_that("ModelCustom - multinomial logit", {

  num_rows <- 100
  num_choices = 30

  my_formula <- ~ x1 + x2 + I(x1^2) + x1:x2
  params = c(`(Intercept)` = 1, x1 = 2.5, x2 = 3, `I(x1^2)` = 0.5 , `x1:x2` = 1)

  test_chooser_data <- data.table(
    id = 1:num_rows,
    sex = sample(c("male", "female"), size = num_rows, replace = T),
    age = sample(1:100, num_rows, replace = T)
  ) %>%
    .[, choiceset := list(list(sample(1:num_choices, size = 5, replace = FALSE))), by = id]

  test_alternative_data <- data.table(
    choice_id = 1:num_choices,
    x1 = runif(num_choices),
    x2 = runif(num_choices)
  )

  multinomial_test_data <-
    test_chooser_data %>%
    unnest_dt(., cols = "choiceset") %>%
    .[, `:=`(choice_id = choiceset, choiceset = NULL)] %>%
    merge(., test_alternative_data, by = "choice_id")

  m <- ModelCustom$new(params = params, formula = my_formula, type = "multinomial")
  res <- m$predict(multinomial_test_data, chooser_id_col = "id", choice_id_col = "choice_id")
  checkmate::expect_data_table(res, any.missing = FALSE, col.names = "strict", ncols = 4)

})
