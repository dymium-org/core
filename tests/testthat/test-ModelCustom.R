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