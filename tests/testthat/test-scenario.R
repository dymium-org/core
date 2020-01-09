test_that("active_scenario", {
  checkmate::expect_list(active_scenario(), types = "character")
  checkmate::expect_access(active_scenario()[['scenario_dir']])
  checkmate::expect_access(active_scenario()[['output_dir']])
  checkmate::expect_access(active_scenario()[['input_dir']])
})
