test_that("get_active_scenario", {
  checkmate::expect_list(get_active_scenario(), types = "character")
  checkmate::expect_access(get_active_scenario()[['scenario_dir']])
  checkmate::expect_access(get_active_scenario()[['output_dir']])
  checkmate::expect_access(get_active_scenario()[['input_dir']])
})
