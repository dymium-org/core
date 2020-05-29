test_that("ModelLinear and ModelBinaryChoice", {

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

  mLinear <- ModelLinear$new(params = params, formula = my_formula)
  checkmate::expect_numeric(mLinear$predict(test_data), finite = T, any.missing = FALSE, len = num_rows)

  mBinaryChoice <- ModelBinaryChoice$new(params = params, formula = my_formula)
  checkmate::expect_numeric(mBinaryChoice$predict(test_data),lower = 0, upper = 1, finite = T, any.missing = FALSE, len = num_rows)

})

test_that("ModelMultinomialLogit", {

  num_rows <- 100
  num_choices = 30

  my_formula <- chosen ~ x1 + x2 + I(x1^2) + x1:x2 + 0
  params = c(x1 = 2.5, x2 = 3, `I(x1^2)` = 0.5 , `x1:x2` = 1)

  test_chooser_data <- data.table(
    id = 1:num_rows,
    sex = sample(c("male", "female"), size = num_rows, replace = T),
    age = sample(1:100, num_rows, replace = T)
  ) %>%
    .[, choiceset := list(list(sample(1:num_choices, size = sample(2:10, 1), replace = FALSE))), by = id]

  test_alternative_data <- data.table(
    choice_id = 1:num_choices,
    x1 = runif(num_choices),
    x2 = runif(num_choices)
  )

  multinomial_test_data <-
    test_chooser_data %>%
    unnest_dt(., cols = "choiceset") %>%
    .[, `:=`(choice_id = choiceset, choiceset = NULL)] %>%
    merge(., test_alternative_data, by = "choice_id") %>%
    .[, chosen := sample(c(T, rep(F, .N - 1))), by = "id"] %>%
    data.table::setcolorder(c("id", "choice_id")) %>%
    data.table::setorder("id")

  m <- ModelMultinomialLogit$new(params = params, formula = my_formula)
  res <- m$predict(multinomial_test_data, chooser_id_col = "id", choice_id_col = "choice_id")

  if (requireNamespace('mlogit')) {
    require("mlogit")
    # no intercept!
    my_formula <- mlogit::mFormula(update(my_formula, ~ . + 0))
    mlogit_model <- mlogit::mlogit(my_formula, data = multinomial_test_data,
                                            choice="chosen",
                                            chid.var="id",
                                            alt.var = "choice_id",
                                            shape = "long")
    m <- ModelMultinomialLogit$new(params = params, formula = my_formula)
    res_mlogit <- m$predict(multinomial_test_data, chooser_id_col = "id", choice_id_col = "choice_id")
    checkmate::expect_data_table(res_mlogit, any.missing = FALSE, col.names = "strict", ncols = 4)
  }

  expect_equal(res_mlogit, res)


})
