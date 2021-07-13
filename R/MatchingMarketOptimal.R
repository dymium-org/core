#' @title Optimal Matching Market
#'
#' @description For general detail See [MatchingMarket]. This class solves matching
#' problems using optimal matching approaches such as one-to-one matching, many-to-one
#' one-sided matching, two-sided matching, etc. This matching strategy assumes
#' agents have perfect information of the market.
#'
#' @usage NULL
#'
#' @format [R6::R6Class] object inheriting from [MatchingMarket].
#' @include MatchingMarket.R
#'
#' #' @section Construction:
#' ```
#' matching_problem = MatchingMarketOptimal$new()
#' ```
#'
#' @section Fields:
#' See [MatchingMarket].
#'
#' @section Methods:
#' See [MatchingMarket].
#'
#' * `simulate(method = c("one-to-one", "many-to-one"), one_sided = FALSE, optimal_A = TRUE, by_group = FALSE, parallel = FALSE)`\cr
#' (`character(1)`, `logical(1)`, `logical(1)`, `logical(1)`, `logical(1)`) -> [data.table::data.table]\cr
#' Simulate using an optimal matching approach.
#'
#'
#' @family MatchingMarket
#' @export
MatchingMarketOptimal <- R6::R6Class(
  classname = "MatchingMarketOptimal",
  inherit = MatchingMarket,
  public = list(
    simulate = function(method = c("one-to-one", "many-to-one"),
                        one_sided = FALSE,
                        optimal_A = TRUE,
                        by_group = FALSE,
                        parallel = FALSE) {
      method <- match.arg(method)
      checkmate::assert_flag(one_sided, na.ok = FALSE, null.ok = FALSE)
      checkmate::assert_flag(optimal_A, na.ok = FALSE, null.ok = FALSE)
      checkmate::assert_flag(by_group, na.ok = FALSE, null.ok = FALSE)
      checkmate::assert_flag(parallel, na.ok = FALSE, null.ok = FALSE)

      if (one_sided) {
        lg$fatal("One-sided has not been implemented.")
        stop()
      }

      parallel_wrapper <- function(...) {
        if (parallel) {
          stopifnot(requireNamespace("furrr"))
          furrr::future_map_dfr(..., .options = furrr::future_options(globals = "self"))
        } else {
          purrr::map_dfr(...)
        }
      }

      if (by_group) {
        markets <- self$split_market(flatten = TRUE)
      } else {
        markets <- self$split_by_n()
      }

      if (!one_sided) {
        if (method == "one-to-one") {
          return(parallel_wrapper(
            .x = markets,
            .f = ~ {
              private$.one_to_one_matching(matching_problem = .x)
            }
          ))
        }
        if (method == "many-to-one") {
          checkmate::assert_names(names(self$matching_problem$agentset_B), must.include = "slots")
          return(parallel_wrapper(
            .x = markets,
            .f = ~ {
              private$.many_to_one_matching(
                matching_problem = .x,
                optimal_A = optimal_A
              )
            }
          ))
        }
      }
    }
  ),
  private = list(
    .check_matching_score_fn = function() {
      scores <-
        self$matching_score_A(
          matching_problem = self$matching_problem,
          idx_A = 1,
          idx_B = c(1, 2)
        )
      checkmate::assert_matrix(scores, null.ok = FALSE, all.missing = FALSE, ncols = 1, nrows = 2)
    },
    .one_to_one_matching = function(matching_problem) {
      stopifnot(all(sapply(matching_problem, is.data.table)))

      n_agentset_A <- nrow(matching_problem$agentset_A)
      n_agentset_B <- nrow(matching_problem$agentset_B)

      # EARLY RETURNS
      if (n_agentset_A == 0 | n_agentset_B == 0) {
        return(private$.early_return(matching_problem))
      }

      # CALCULATE SCORES
      utils_A <-
        self$matching_score_A(
          matching_problem,
          idx_A = c(1:n_agentset_A),
          idx_B = c(1:n_agentset_B)
        )
      utils_B <-
        self$matching_score_B(
          matching_problem,
          idx_B = c(1:n_agentset_B),
          idx_A =  c(1:n_agentset_A)
        )

      # SOLVE OPTIMAL MATCHING
      match_result <-
        matchingR::galeShapley.marriageMarket(
          proposerUtils = utils_A,
          reviewerUtils = utils_B
        )

      # POST-PROCESS MATCH RESULT
      if (n_agentset_A >= n_agentset_B) {
        match_result_dt <- data.table(
          id_A = matching_problem$agentset_A[[self$matching_problem$id_col_A]],
          id_B = matching_problem$agentset_B[[self$matching_problem$id_col_B]][as.vector(match_result$proposals)]
        )
      } else {
        match_result_dt <- data.table(
          id_A = matching_problem$agentset_A[[self$matching_problem$id_col_A]][as.vector(match_result$engagements)],
          id_B = matching_problem$agentset_B[[self$matching_problem$id_col_B]]
        )
      }

      return(match_result_dt)
    },
    .many_to_one_matching = function(matching_problem, optimal_A = TRUE) {
      stopifnot(all(sapply(matching_problem, is.data.table)))
      checkmate::assert_flag(optimal_A, na.ok = FALSE, null.ok = FALSE)

      n_agentset_A <- nrow(matching_problem$agentset_A)
      n_agentset_B <- nrow(matching_problem$agentset_B)

      # EARLY RETURNS
      if (n_agentset_A == 0 | n_agentset_B == 0) {
        return(private$.early_return(matching_problem))
      }

      # CALCULATE SCORES
      utils_A <-
        self$matching_score_A(
          matching_problem,
          idx_A = c(1:n_agentset_A),
          idx_B = c(1:n_agentset_B)
        )
      utils_B <-
        self$matching_score_B(
          matching_problem,
          idx_B = c(1:n_agentset_B),
          idx_A =  c(1:n_agentset_A)
        )

      # SOLVE OPTIMAL MATCHING
      match_result <-
        matchingR::galeShapley.collegeAdmissions(
          studentUtils = utils_A,
          collegeUtils = utils_B,
          slots = matching_problem$agentset_B[["slots"]],
          studentOptimal = optimal_A
        )

      # POST-PROCESS MATCH RESULT
      match_result_dt <-
        data.table(
          id_A = matching_problem$agentset_A[[self$matching_problem$id_col_A]],
          id_B = matching_problem$agentset_B[[self$matching_problem$id_col_B]][match_result$matched.students]
        )

      return(match_result_dt)
    }
  )
)
