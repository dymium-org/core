test_that("ModelMultinomialLogit", {
  if (requireNamespace('mlogit')) {

    data("Fishing", package = "mlogit")

    # fitting
    .data_dfidx <- dfidx::dfidx(Fishing, varying = 2:9, shape = "wide", choice = "mode")
    mod <- mlogit::mlogit(mode ~ price + catch, data = .data_dfidx)

    # data for prediction
    .data <- dfidx::unfold_idx(.data_dfidx)

    Mod <- ModelMultinomialLogit$new(params = mod$coefficients, formula = mod$formula)

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
