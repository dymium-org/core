test_that("makeModel", {

  # binary choice model
  bc_model <- create_caret_binary_model()
  Mod <- makeModel(bc_model)
  checkmate::expect_r6(Mod, classes = "ModelBinaryChoice")

  # mlogit model
  mlogit_model <- create_mlogit_model()
  Mod <- makeModel(mlogit_model)
  checkmate::expect_r6(Mod, classes = "ModelMultinomialLogit")

})
