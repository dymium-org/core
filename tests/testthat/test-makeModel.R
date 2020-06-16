test_that("makeModel", {

  # binary choice model
  bc_model <- create_caret_binary_model()
  Mod <- makeModel(bc_model)
  checkmate::expect_r6(Mod,classes = "ModelBinaryChoice")

  # multinomial choice model
  # mnl_model <- create_caret_multinomial_model()
  # Mod <- makeModel(mnl_model)

})
