test_that("initialise Entity2", {
  Entity2$new(databackend = DataBackendDataTable, .data = toy_individuals, id_col = "pid")
})
