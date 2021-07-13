test_that("Entity2 works", {

# constructor -------------------------------------------------------------

  en = Entity2$new(databackend = DataBackendDataTable, .data = toy_individuals, id_col = "pid")


# add ---------------------------------------------------------------------



# get_data ----------------------------------------------------------------

  checkmate::expect_data_table(en$get_data(), nrows = nrow(toy_individuals))

})
