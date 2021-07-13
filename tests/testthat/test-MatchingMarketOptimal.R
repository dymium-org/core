test_that("multiplication works", {
  MarriageMarketOptimal <- R6::R6Class(
    classname = "MarriageMarketOptimal",
    inherit = MatchingMarketOptimal,
    public = list(
      matching_score_A = function(matching_problem, idx_A, idx_B) {
        outer(
          X = matching_problem$agentset_B[["age"]][idx_B], # reviewers, rows
          Y = matching_problem$agentset_A[["age"]][idx_A], # proposers, columns
          function(x, y) {
            1 / (1 + abs(x - y))
          }
        )
      },
      matching_score_B = function(matching_problem, idx_B, idx_A) {
        outer(
          X = matching_problem$agentset_A[["age"]][idx_A], # reviewers, rows
          Y = matching_problem$agentset_B[["age"]][idx_B], # proposers, columns
          function(x, y) {
            1 / (1 + abs(x - y))
          }
        )
      }
    )
  )

  create_toy_population()
  Ind <- pop$get("Individual")
  Hh <- pop$get("Household")

  context("matching market optimal: one-to-one")
  MarriageMarket <-
    MarriageMarketOptimal$new(
      agentset_A = Ind$get_data()[sex == IND$SEX$MALE],
      agentset_B = Ind$get_data()[sex == IND$SEX$FEMALE],
      slots_B = sample(c(1:3), size = Ind$get_data()[sex == IND$SEX$FEMALE, .N], replace = TRUE)
      # grouping_vars = c("marital_status")
      # max_market_size = 40
    )

  optimal_matches_one_to_one <-
    MarriageMarket$simulate(one_sided = FALSE)
  checkmate::expect_data_table(optimal_matches_one_to_one,
    null.ok = FALSE,
    ncols = 2,
    min.rows = ifelse(nrow(Ind$get_data()[sex == IND$SEX$MALE]) <= nrow(Ind$get_data()[sex == IND$SEX$FEMALE]),
      nrow(Ind$get_data()[sex == IND$SEX$FEMALE]),
      nrow(Ind$get_data()[sex == IND$SEX$MALE])
    )
  )

  context("matching market optimal: many-to-one")
  n_B <- 20
  MarriageMarket <-
    MarriageMarketOptimal$new(
      agentset_A = Ind$get_data()[sex == IND$SEX$MALE],
      agentset_B = Ind$get_data()[sex == IND$SEX$FEMALE][1:n_B],
      slots_B = sample(c(1:3), size = 20, replace = TRUE)
      # grouping_vars = c("marital_status")
      # max_market_size = 40
    )

  optimal_matches_many_to_one <-
    MarriageMarket$simulate(one_sided = FALSE, method = "many-to-one")
  checkmate::expect_data_table(optimal_matches_many_to_one, null.ok = FALSE, ncols = 2, min.rows = 1)
  expect_true(optimal_matches_many_to_one[, uniqueN(id_B, na.rm = T)] == n_B)
})
