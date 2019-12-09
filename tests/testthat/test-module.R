test_that("check_module", {
  # check_module("demography")
})

test_that("get_modules", {
  # get_modules()
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
