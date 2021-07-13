test_that("ModelMultinomialLogit", {
  if (requireNamespace('mlogit')) {

    data("Fishing", package = "mlogit")

    # fitting
    .data_dfidx <- dfidx::dfidx(Fishing, varying = 2:9, shape = "wide", choice = "mode")
    mod <- mlogit::mlogit(mode ~ price + catch, data = .data_dfidx)

    # data for prediction
    .data <- dfidx::unfold_idx(.data_dfidx)
    params = as.numeric(mod$coefficients)
    names(params) = names(mod$coefficients)
    Mod <- ModelMultinomialLogit$new(params = params, formula = mod$formula)
    Mod_formula <- ModelMultinomialLogit$new(params = mod$coefficients, formula = mode ~ price + catch)

    # compare predictions
    prediction_from_mlogit <-
      predict(mod, .data_dfidx) %>%
      as.data.table()
    prediction_from_Mod <-
      Mod$predict(.data, chooser_id_col = "id1", choice_id_col = "id2") %>%
      data.table::dcast(chooser_id ~ choice_id, value.var = "prob") %>%
      .[, -"chooser_id"]
    prediction_from_Mod_formula <- Mod_formula$predict(.data,
                                                       chooser_id_col = "id1",
                                                       choice_id_col = "id2") %>%
      data.table::dcast(chooser_id ~ choice_id, value.var = "prob") %>%
      .[, -"chooser_id"]
    prediction_from_Mod_using_dfidx <-
      Mod$predict(.data_dfidx, chooser_id_col = "id1", choice_id_col = "id2") %>%
      data.table::dcast(chooser_id ~ choice_id, value.var = "prob") %>%
      .[, -"chooser_id"]

    expect_true(all.equal(prediction_from_mlogit, prediction_from_Mod))
    expect_true(all.equal(prediction_from_mlogit, prediction_from_Mod_formula))
    expect_true(all.equal(prediction_from_mlogit, prediction_from_Mod_using_dfidx))

  }
})

test_that("ModelMultinomialLogit - Pure multinomial logit", {
  if (requireNamespace('mlogit')) {

    data("Fishing", package = "mlogit")

    # fitting
    form = mode ~ 0 | income
    .data_dfidx <- dfidx::dfidx(Fishing, varying = 2:9, shape = "wide", choice = "mode")
    mod <- mlogit::mlogit(form, data = .data_dfidx)

    # data for prediction
    .data <- dfidx::unfold_idx(.data_dfidx)
    params = as.numeric(mod$coefficients)
    names(params) = names(mod$coefficients)
    Mod <- ModelMultinomialLogit$new(params = params, formula = mod$formula)
    Mod_formula <- ModelMultinomialLogit$new(params = mod$coefficients, formula = form)

    # compare predictions
    prediction_from_mlogit <-
      predict(mod, .data_dfidx) %>%
      as.data.table()
    prediction_from_Mod <-
      Mod$predict(.data, chooser_id_col = "id1", choice_id_col = "id2") %>%
      data.table::dcast(chooser_id ~ choice_id, value.var = "prob") %>%
      .[, -"chooser_id"]
    prediction_from_Mod_formula <- Mod_formula$predict(.data,
                                                       chooser_id_col = "id1",
                                                       choice_id_col = "id2") %>%
      data.table::dcast(chooser_id ~ choice_id, value.var = "prob") %>%
      .[, -"chooser_id"]
    prediction_from_Mod_using_dfidx <-
      Mod$predict(.data_dfidx, chooser_id_col = "id1", choice_id_col = "id2") %>%
      data.table::dcast(chooser_id ~ choice_id, value.var = "prob") %>%
      .[, -"chooser_id"]

    expect_true(all.equal(prediction_from_mlogit, prediction_from_Mod))
    expect_true(all.equal(prediction_from_mlogit, prediction_from_Mod_formula))
    expect_true(all.equal(prediction_from_mlogit, prediction_from_Mod_using_dfidx))

  }
})

test_that("ModelMultinomialLogit - different alternatives", {
  num_rows <- 10000
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
    data.table::setorder("id") %>%
    as.data.frame()

  m <- ModelMultinomialLogit$new(params = params, formula = my_formula)
  prediction <- m$predict(multinomial_test_data, chooser_id_col = "id", choice_id_col = "choice_id")
  checkmate::expect_data_table(prediction, any.missing = FALSE, col.names = "strict", ncols = 4)

})
