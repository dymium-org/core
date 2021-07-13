test_that("alignment", {
  p <- data.table(yes = runif(100))[, no := 1 - yes]
  t <- list(yes = 10, no = 20)
  a <- alignment(p, t)
  res <- table(a)
  expect_equal(res[["no"]], t$no)
  expect_equal(res[["yes"]], t$yes)
  expect_equal(table(is.na(a))[["TRUE"]], nrow(p) - sum(unlist(t)))

  t <- list(yes = 100, no = 20)
  expect_error(
    alignment(p, t),
    "The sum of targets cannot exceed the number of agents that are undergoing this transition."
  )

  t <- list(yes = 10, no = 20, unknown = 20)
  expect_error(
    alignment(p, t),
    "Assertion on 'names\\(target\\)' failed: Must be a subset of set \\{yes,no\\}"
  )
})
