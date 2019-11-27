context("module")

test_that("event no overwriting", {
  # create script
  create_new_event(event_name = "age",
                   module_name = "demography",
                   module_path = tempdir())

  # try to overwrite
  expect_error(create_new_event(event_name = "age",
                                module_name = "demography",
                                module_path = tempdir(),
                                        filename = "age"))
})

test_that("event no white spaces", {
  expect_error(create_new_event(event_name = "white space",
                                module_name = "demography",
                                module_path = tempdir()))

  expect_error(create_new_event(event_name = "test",
                                module_name = "white space",
                                module_path = tempdir()))
})


test_that("module no overwriting", {
  create_new_module(name = "testModule",
                    event = c("event1", "event2"),
                    path = tempdir())

  expect_error(create_new_module(name = "testModule",
                                 event = c("event1", "event2"),
                                 path = tempdir()))
})
