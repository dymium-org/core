test_that("check_module", {
  expect_true(check_module("demography"))
})

test_that("get_modules", {
  checkmate::expect_names(get_modules(), must.include = c("demography", "matsim", "test"))
})

test_that(".filter_zip_versions", {
  name <- "demography"
  module_files <- c("modules/demography/readme.txt",
                    "modules/demography/event1.R",
                    "modules/demography/demography_1.0.0.zip",
                    "modules/demography/demography_1.2.0.zip",
                    "modules/demography/demography_dummy.zip",
                    "modules/demography/demography_dummy.1.2",
                    "modules/demography/demography_1.3.0/",
                    "modules/demography/demography_zip")
  expect_length(.filter_zip_versions(x = module_files, name), n = 3)
})

test_that("get_module", {
  expect_error(download_module(name = "test", version = "10.0.0", force = T, remove_download = T),
               regexp = "The requested version of test module doesn't exist.")
})
