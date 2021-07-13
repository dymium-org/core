test_that("initialise", {
  MarriageMarketStochastic <- R6::R6Class(
    classname = "MarriageMarketStochastic",
    inherit = MatchingMarketStochastic,
    public = list(
      matching_score_A = function(matching_problem = self$matching_problem, idx_A, idx_B) {
        scores <- 1 / (1 + abs(
          matching_problem$agentset_A[["age"]][idx_A] -
            matching_problem$agentset_B[["age"]][idx_B]
        ))
      },
      matching_score_B = function(matching_problem = self$matching_problem, idx_B, idx_A) {
        scores <- 1 / (1 + abs(
          matching_problem$agentset_B[["age"]][idx_B] -
            matching_problem$agentset_A[["age"]][idx_A]
        ))
      }
    )
  )

  create_toy_population()
  Ind <- pop$get("Individual")
  Hh <- pop$get("Household")

  MarriageMarket <-
    MarriageMarketStochastic$new(
      agentset_A = Ind$get_data()[sex == IND$SEX$MALE],
      agentset_B = Ind$get_data()[sex == IND$SEX$FEMALE],
      grouping_vars = c("marital_status"),
      max_market_size = 50
    )

  x_rand <- MarriageMarket$simulate(n_choices = 10, by_group = TRUE, parallel = FALSE)
  x_rand <- MarriageMarket$simulate(n_choices = 10, by_group = TRUE, parallel = TRUE)
  x_rank <- MarriageMarket$simulate(n_choices = 10, by_group = TRUE, parallel = FALSE, method = "ranking")

  check_matching <- function(x) {
    # EACH ID OF THE SAME AGENT TYPE ONLY APPEAR ONE IN EITHER OF THE COLUMNS
    expect_true(sum(x[!is.na(id_A), id_A] %in% x[!is.na(id_B), id_B]) == 0)
    expect_true(sum(x[!is.na(id_B), id_B] %in% x[!is.na(id_A), id_A]) == 0)
  }
  check_matching(x_rand)
  check_matching(x_rank)

  if (FALSE) {
    x_rand %>%
      .[pop$ind$get_data(), , on = .(id = pid), nomatch = 0] %>%
      .[pop$ind$get_data(), , on = .(cid = pid), nomatch = 0] %>%
      .[, age_diff := age - i.age] %>%
      .[, hist(age_diff, breaks = 20)]

    x_rank %>%
      .[pop$ind$get_data(), , on = .(id = pid), nomatch = 0] %>%
      .[pop$ind$get_data(), , on = .(cid = pid), nomatch = 0] %>%
      .[, age_diff := age - i.age] %>%
      .[, hist(age_diff, breaks = 20)]
  }
})
