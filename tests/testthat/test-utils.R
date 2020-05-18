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

test_that("omit_derived_vars", {
  .df <- data.frame(a = rep(10, 10), b = rep(10, 10), .c = rep(10, 10), d.d = rep(10, 10))
  .dt <- data.table(a = rep(10, 10), b = rep(10, 10), .c = rep(10, 10), d.d = rep(10, 10))
  expect_equal(names(omit_derived_vars(.df)), c("a", "b", "d.d"))
  expect_equal(names(omit_derived_vars(.dt)), c("a", "b", "d.d"))
})

test_that("normalise_derived_vars", {
  .df <- data.frame(a = rep(10, 10), b = rep(10, 10), .c = rep(10, 10), d.d = rep(10, 10))
  .dt <- data.table::data.table(a = rep(10, 10), b = rep(10, 10), .c = rep(10, 10), d.d = rep(10, 10))
  expect_equal(names(normalise_derived_vars(.df)), expected = c("a", "b", "c", "d.d"))
  expect_equal(names(normalise_derived_vars(.dt)), expected = c("a", "b", "c", "d.d"))
})

test_that("sample_choice", {
  expect_setequal(sample_choice(10, 1, replace, replace = T), sample_choice(10, 1))
  expect_setequal(sample_choice(1, 10, replace = T), rep(1,10))
  expect_setequal(sample_choice(1, 1, replace = T), 1)
  expect_error(sample_choice(1, 10),
               regexp = "cannot take a sample larger than the population when 'replace = FALSE'")
})

test_that("lookup_and_replace2", {
  dt <- data.table(
    pid = c(1L, 2L, 3L, 4L, 5L, 6L),
    hid = c(1L, 1L, 2L, 2L, 2L, 2L),
    partner_id = c(2L, 1L, NA, NA, 6L, 5L)
  )

  lookup_table <- data.frame(.key = c(1:6),
                             .value = c(7:12))

  res <- lookup_and_replace2(x = dt,
                             cols = c("pid", "partner_id"),
                             mapping = lookup_table)

  expected_res <- data.table(
    pid = c(7:12),
    hid = c(1, 1, 2, 2, 2, 2),
    partner_id = c(8, 7, NA, NA, 12, 11)
  )
  expect_true(all.equal(res, expected_res))
})


test_that("lookup_and_replace2 - as character", {
  dt <- data.table::data.table(
    pid = c(1L, 2L, 3L, 4L, 5L, 6L),
    hid = c(1L, 1L, 2L, 2L, 2L, 2L),
    partner_id = c(2L, 1L, NA, NA, 6L, 5L)
  ) %>%
    .[, lapply(.SD, as.character)]

  lookup_table <- data.frame(.key = as.character(c(1:6)),
                             .value = c(7:12))

  res <- lookup_and_replace2(x = dt,
                             cols = c("pid", "partner_id"),
                             mapping = lookup_table)

  str(res)

  expected_res <- data.table(
    pid = c(7:12),
    hid = as.character(c(1, 1, 2, 2, 2, 2)),
    partner_id = c(8, 7, NA, NA, 12, 11)
  )
  expect_true(all.equal(res, expected_res))
})

test_that("dsample", {

  expect_equal(dsample(10, size = 10, replace = T), rep(10, 10))
  expect_equal(dsample(10, size = 10, replace = T, prob = 1), rep(10, 10))

})
