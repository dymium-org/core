test_that("Initialise", {

  create_toy_population()
  Ind <- pop$get("Individual")
  Hh <- pop$get("Household")

  # impute fake zones
  n_zones <- 5
  Ind$get_data(copy = FALSE)[, zid := sample(1:n_zones, .N, replace = TRUE)]

  Market <-
    MatchingMarket$new(
      agentset_A = Ind$get_data()[sex == IND$SEX$MALE],
      agentset_B = Ind$get_data()[sex == IND$SEX$FEMALE],
      grouping_vars = c("zid"),
      max_market_size = 10
    )

  checkmate::expect_list(Market$split_market(), len = n_zones, null.ok = FALSE)
  checkmate::expect_list(Market$split_by_n(n_submarkets = 10), len = 10, null.ok = FALSE)
  checkmate::expect_list(Market$split_by_group(), len = n_zones, null.ok = FALSE)
})


# test_that("Matching benchmarks", {
#
#   if (FALSE) {
#     get_matches <- function(match_result) {
#       match_id <- data.table(
#         id = match_result$dat[["id"]][match_result$chooser_idx_pool],
#         pid = match_result$dat[["id"]][match_result$matched_partner_idx])
#
#       matches <- match_id[match_result$dat, , on = "id"] %>%
#         .[match_result$dat, , on = .(pid = id), nomatch = 0]
#
#       # NO OVER LAPPING
#       expect_true(nrow(matches)*2 == matches[, uniqueN(c(id, pid))])
#
#       matches
#     }
#
#     # pweighted --------------------
#     pweighted_match_result <- rand_match(10000, 30, method = "pweighted") %>%
#       get_matches()
#
#     pweighted_match_result[, hist(age - i.age)]
#
#     # ranking -----------------------
#     ranking_match_result <- rand_match(10000, 30, method = "ranking")  %>%
#       get_matches()
#
#     ranking_match_result[, hist(age - i.age)]
#
#     # benchmarks
#     rbenchmark::benchmark(
#       pweighted = rand_match(10000, 30, method = "pweighted"),
#       ranking = rand_match(10000, 30, method = "ranking"),
#       replications = 5
#     )
#
#
#     rbenchmark::benchmark(
#       rand1 = rand_match(1000, 30),
#       rand2 = rand_match2(1000, 30),
#       # n10000 = rand_match(10000, 30),
#       # n50000 = rand_match(50000, 30),
#       replications = 10
#       # n30000 = rand_match(100,10),
#     )
#
#     n_varying <-
#       rbenchmark::benchmark(
#         n100 = rand_match(100, 30),
#         n1000 = rand_match(1000, 30),
#         n10000 = rand_match(10000, 30),
#         # n50000 = rand_match(50000, 30),
#         replications = 10
#         # n30000 = rand_match(100,10),
#       )
#
#     system.time(rand_match(50000, 30))
#
#     n_choices_varying <-
#       rbenchmark::benchmark(
#         n_choices10 = rand_match(10000, 10),
#         n_choices100 = rand_match(10000, 100),
#         n_choices1000 = rand_match(10000, 1000),
#         n_choices1000 = rand_match(10000, 1000),
#         # n50000 = rand_match(50000,10),
#         replications = 10
#         # n30000 = rand_match(100,10),
#       )
#
#   }
#
# })
