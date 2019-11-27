# context("test-matching")
#
# test_that("MatchingMarket, toy data #1 with one single market", {
#   # synthesise test data
#   proposer_ids <- c(1:3)
#   proposee_ids <- c(4:5)
#   pr_var <- c(9,10,4)
#   pe_var <- c(10,4)
#   # outer(pe_var, pr_var, function(x,y){
#   #   1 / (1 + abs(x - y))
#   # })
#
#   # proposer_utils <-
#   #   matrix(
#   #     data = c(10,7,12,4,6,8), byrow = FALSE,
#   #     nrow = length(proposee_ids), ncol = length(proposer_ids))
#   # proposee_utils <- t(proposer_utils)
#
#   mm <- MatchingMarket$new(max_market_size = 10)
#   mm$load_market_data(
#     proposer_ids = proposer_ids,
#     proposee_ids = proposee_ids,
#     proposer_var = pr_var,
#     proposee_var = pe_var)
#   matches <- mm$two_sided_matching()
#   # mm$summary()
#
#   expected_res <- data.table(
#     proposer_ids = c(1,2,3),
#     proposee_ids = c(NA,4,5))
#
#   expect_true(nrow(matches) == 3)
#   expect_equal(matches[order(proposer_ids)], expected_res)
# })
#
# test_that("MatchingMarket, toy data #1 with one single market and util matrices as input", {
#   # synthesise test data
#   proposer_ids <- c(1:3)
#   proposee_ids <- c(4:5)
#
#   proposer_utils <-
#     matrix(
#       data = c(10,7,12,4,6,8), byrow = FALSE,
#       nrow = length(proposee_ids), ncol = length(proposer_ids))
#   proposee_utils <- t(proposer_utils)
#
#   mm <- MatchingMarket$new(max_market_size = 10)
#   mm$load_market_data(
#     proposer_ids = proposer_ids,
#     proposee_ids = proposee_ids,
#     proposee_utils = proposee_utils,
#     proposer_utils = proposer_utils)
#   matches <- mm$two_sided_matching()
#   # mm$summary()
#
#   expected_res <- data.table(
#     proposer_ids = c(1,2,3),
#     proposee_ids = c(NA,4,5))
#
#   expect_true(nrow(matches) == 3)
#   expect_equal(matches[order(proposer_ids)], expected_res)
# })
#
# test_that("MatchingMarket, toy data #2 with one single market", {
#   # synthesise test data
#   proposer_ids <- c(1:3)
#   proposee_ids <- c(4:6)
#   pr_var <- c(9,10,4)
#   pe_var <- c(10,4,1)
#
#   mm <- MatchingMarket$new(max_market_size = 10)
#   mm$load_market_data(
#     proposer_ids = proposer_ids,
#     proposee_ids = proposee_ids,
#     proposer_var = pr_var,
#     proposee_var = pe_var)
#   matches <- mm$two_sided_matching()
#   # mm$summary()
#
#   expected_res <- data.table(
#     proposer_ids = c(1,2,3),
#     proposee_ids = c(6,4,5))
#
#   expect_true(nrow(matches) == 3)
#   expect_equal(matches[order(proposer_ids)], expected_res)
# })
#
#
# test_that("MatchingMarket, with one pair", {
#   # synthesise test data
#   set.seed(1)
#   all_agents <- seq_len(2)
#   proposer_ids <- sample(all_agents, size = length(all_agents) / 2)
#   proposee_ids <- all_agents[!all_agents %in% proposer_ids]
#   proposer_utils <-
#     matrix(
#       data = runif(length(proposer_ids) * length(proposee_ids)),
#       byrow = FALSE,
#       nrow = length(proposee_ids),
#       ncol = length(proposer_ids))
#   proposee_utils <-
#     matrix(
#       data = runif(length(proposer_ids) * length(proposee_ids)),
#       byrow = FALSE,
#       nrow = length(proposer_ids),
#       ncol = length(proposee_ids))
#
#
#   # mm <- MatchingMarket$new(max_market_size = ceiling((length(all_agents) / 2)^2 / 5))
#   mm <- MatchingMarket$new(max_market_size = 4)
#   mm$load_market_data(
#     proposer_ids = proposer_ids,
#     proposee_ids = proposee_ids,
#     proposer_utils = proposer_utils,
#     proposee_utils = proposee_utils)
#   # mm$get_submarkets()
#   # mm$summary()
#
#   matches <- mm$two_sided_matching()
#
#   expect_true(nrow(matches) == 1)
#   expect_true(!all(is.na(matches)))
# })
#
# test_that("MatchingMarket, with one non-match", {
#   # synthesise test data
#   set.seed(1)
#   all_agents <- seq_len(999)
#   proposer_ids <- sample(all_agents, size = length(all_agents) / 2)
#   proposee_ids <- all_agents[!all_agents %in% proposer_ids]
#   proposer_utils <-
#     matrix(
#       data = runif(length(proposer_ids) * length(proposee_ids)),
#       byrow = FALSE,
#       nrow = length(proposee_ids),
#       ncol = length(proposer_ids))
#   proposee_utils <-
#     matrix(
#       data = runif(length(proposer_ids) * length(proposee_ids)),
#       byrow = FALSE,
#       nrow = length(proposer_ids),
#       ncol = length(proposee_ids))
#
#
#   # mm <- MatchingMarket$new(max_market_size = ceiling((length(all_agents) / 2)^2 / 5))
#   mm <- MatchingMarket$new(max_market_size = 100^2)
#   mm$load_market_data(
#     proposer_ids = proposer_ids,
#     proposee_ids = proposee_ids,
#     proposer_utils = proposer_utils,
#     proposee_utils = proposee_utils
#   )
#   # mm$get_submarkets()
#
#   matches <- mm$two_sided_matching()
#
#   expect_true(table(is.na(matches))[['TRUE']] == 1)
# })
#
# test_that("MatchingMarket, with unequal size of proposers and reviewers", {
#   # synthesise test data
#   set.seed(1)
#   proposer_ids <- c(1:1000)
#   proposee_ids <- c(1250:2000)
#   proposer_utils <-
#     matrix(
#       data = runif(length(proposer_ids) * length(proposee_ids)),
#       byrow = FALSE,
#       nrow = length(proposee_ids),
#       ncol = length(proposer_ids))
#   proposee_utils <-
#     matrix(
#       data = runif(length(proposer_ids) * length(proposee_ids)),
#       byrow = FALSE,
#       nrow = length(proposer_ids),
#       ncol = length(proposee_ids))
#
#
#   # mm <- MatchingMarket$new(max_market_size = ceiling((length(all_agents) / 2)^2 / 5))
#   mm <- MatchingMarket$new(max_market_size = 200^2)
#   mm$load_market_data(
#     proposer_ids = proposer_ids,
#     proposee_ids = proposee_ids,
#     proposer_utils = proposer_utils,
#     proposee_utils = proposee_utils)
#   # mm$get_submarkets()
#   # mm$summary()
#
#   matches <- mm$two_sided_matching()
#
#   expect_true(table(is.na(matches))[['TRUE']] == (length(proposer_ids) - length(proposee_ids)))
# })
#
# test_that("MatchingMarket, more proposees than proposers", {
#   # synthesise test data
#   set.seed(1)
#   proposee_ids <- c(1:1000)
#   proposer_ids <- c(1250:2000)
#   proposer_utils <-
#     matrix(
#       data = runif(length(proposer_ids) * length(proposee_ids)),
#       byrow = FALSE,
#       nrow = length(proposee_ids),
#       ncol = length(proposer_ids))
#   proposee_utils <-
#     matrix(
#       data = runif(length(proposer_ids) * length(proposee_ids)),
#       byrow = FALSE,
#       nrow = length(proposer_ids),
#       ncol = length(proposee_ids))
#
#
#   # mm <- MatchingMarket$new(max_market_size = ceiling((length(all_agents) / 2)^2 / 5))
#   mm <- MatchingMarket$new(max_market_size = 200^2)
#   mm$load_market_data(
#     proposer_ids = proposer_ids,
#     proposee_ids = proposee_ids,
#     proposer_utils = proposer_utils,
#     proposee_utils = proposee_utils)
#   # mm$get_submarkets()
#   # mm$summary()
#
#   matches <- mm$two_sided_matching()
#
#   expect_true(table(is.na(matches))[['TRUE']] == abs(length(proposer_ids) - length(proposee_ids)))
# })
#
#
# # test_that("MatchingMarket, large number of agents", {
# #   # synthesise test data
# #   set.seed(1)
# #   proposee_ids <- c(1:10000)
# #   proposer_ids <- c(20000:30000)
# #   proposer_utils <-
# #     matrix(
# #       data = runif(length(proposer_ids) * length(proposee_ids)),
# #       byrow = FALSE,
# #       nrow = length(proposee_ids),
# #       ncol = length(proposer_ids))
# #   proposee_utils <-
# #     matrix(
# #       data = runif(length(proposer_ids) * length(proposee_ids)),
# #       byrow = FALSE,
# #       nrow = length(proposer_ids),
# #       ncol = length(proposee_ids))
# #
# #
# #   # mm <- MatchingMarket$new(max_market_size = ceiling((length(all_agents) / 2)^2 / 5))
# #   mm <- MatchingMarket$new(max_market_size = 500^2)
# #   mm$load_market_data(
# #     proposer_ids = proposer_ids,
# #     proposee_ids = proposee_ids,
# #     proposer_utils = proposer_utils,
# #     proposee_utils = proposee_utils)
# #   mm$get_submarkets()
# #   mm$summary()
# #
# #   matches <- mm$two_sided_matching()
# # })
