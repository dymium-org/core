test_that("condense_rows()", {
  x <- c(1,2,3,4,4)
  y <- c(3,7,4,2,6)
  z <- condense_rows(x, y)
  expect_that(nrow(z), equals(length(unique(x))))
  expect_true(all(y %in% z[, unlist(target)]))
})


test_that("lookup_and_replace", {
  dt <- data.table(
    pid = c(1L, 2L, 3L, 4L, 5L, 6L),
    hid = c(1L, 1L, 2L, 2L, 2L, 2L),
    partner_id = c(2L, 1L, NA, NA, 6L, 5L),
    children_ids = list(integer(), integer(), integer(), integer(), c(4L, 3L), c(3L, 4L))
    # house_ids = list(integer(), integer(), integer(), integer(), c(4L, 3L), c(3L, 4L))
  )

  lookup_table <- data.frame(key = c(1:6),
    value = c(7:12))

  res <- lookup_and_replace(data = dt,
    lookup_table = lookup_table,
    cols = c("pid", "partner_id", "children_ids"),
    id_col = "pid")

  expected_res <- data.table(
    pid = c(7:12),
    hid = c(1, 1, 2, 2, 2, 2),
    partner_id = c(8, 7, NA, NA, 12, 11),
    children_ids = list(integer(), integer(), integer(), integer(), c(10L, 9L), c(9L, 10L))
  )

  expect_equal(res, expected_res)
})
