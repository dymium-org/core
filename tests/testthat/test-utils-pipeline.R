test_that("extract_data", {
  create_toy_world()
  checkmate::expect_list(extract_data(world),
    types = "data.table", names = "unique"
  )
  checkmate::expect_list(extract_data(world$entities$Individual),
    types = "data.table", names = "unique"
  )
  checkmate::expect_list(extract_data(world$entities$Individual$database$attrs),
    types = "data.table", names = "unique"
  )
})
