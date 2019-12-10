#' @title Stochastic Matching Market
#'
#' @description For general detail See [MatchingMarket]. This class solves a matching
#' problem using a stochastic strategy such as randomly pick based on probabilistic scores
#' or ranking. This stretegy mimics cases where agents have imperfect imformation about
#' the market hence it only awares of a subset of all the available options in the market.
#'
#' @usage NULL
#' @format [R6::R6Class] object inheriting from [MatchingMarket].
#' @include MatchingMarket.R
#'
#' #' @section Construction:
#' ```
#' matching_problem = MatchingMarketStochastic$new()
#' ```
#' For a description of the arguments, see [Learner].
#' `task_type` is set to `"classif"`.
#'
#' @section Fields:
#' See [MatchingMarket].
#'
#' @section Methods:
#' See [MatchingMarket].
#'
#' * `simulate(n_choices = 10, method = c("pweighted", "ranking"), by_group = FALSE, parallel = FALSE)`\cr
#'  (`integer(1)`, `character(1)`, `logical(1)`, `logical(1)`) -> `[data.table::data.table()]`\cr
#'  Simulate matching stochastically.
#'
#' @family MatchingMarket
#' @export
MatchingMarketStochastic <- R6::R6Class(
  classname = "MatchingMarketStochastic",
  inherit = MatchingMarket,
  public = list(
    simulate = function(n_choices = 10,
                        method = "pweighted",
                        by_group = FALSE,
                        parallel = FALSE) {
      checkmate::assert_flag(by_group, na.ok = FALSE, null.ok = FALSE)
      checkmate::assert_flag(parallel, na.ok = FALSE, null.ok = FALSE)
      assert_that(
        is.null(self$matching_problem$slots_B),
        msg = self$message(
          "Stochastic matching is currently not supported",
          "many-to-one matching problem. Please create a new",
          "MatchingMarket object without giving any `slots_B` value",
          "to use `stochastic_matching` method. Else use `optimal_matching`",
          "method for solving many-to-one matching."
        )
      )

      if (parallel & isFALSE(by_group)) {
        self$message("`parallel` is ignored. ",
                     "Currently, `parallel` only works when by_group is TRUE.")
      }

      if (by_group) {
        markets <- self$split_market(flatten = TRUE)
      } else {
        # make sure the size of each market doesn't exceed self$max_market_size
        markets <- self$split_by_n()
      }

      parallel_wrapper <- function(...) {
        if (parallel) {
          stopifnot(requireNamespace('furrr'))
          furrr::future_map_dfr(..., .options = furrr::future_options(globals = "self"))
        } else {
          purrr::map_dfr(...)
        }
      }

      parallel_wrapper(.x = markets,
                       .f = ~ {
                         private$.stochastic_matching(
                           matching_problem = .x,
                           n_choices = n_choices,
                           method = method
                         )
                       })
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

      checkmate::assert_numeric(
        scores,
        finite = T,
        any.missing = FALSE,
        null.ok = FALSE,
        len = 2
      )

    },

    .stochastic_matching =
      function(matching_problem,
               n_choices = 10,
               method = c("pweighted", "ranking")) {
        # CHECK INPUTS
        checkmate::assert_data_table(matching_problem$agentset_A, null.ok = FALSE)
        checkmate::assert_data_table(matching_problem$agentset_B, null.ok = FALSE)
        checkmate::assert_count(n_choices, na.ok = FALSE, positive = T, null.ok = FALSE)
        method <- match.arg(method)

        n_agentset_A <- nrow(matching_problem$agentset_A)
        n_agentset_B <- nrow(matching_problem$agentset_B)

        # EARLY RETURN
        if (n_agentset_A == 0 | n_agentset_B == 0) {
          return(private$.early_return(matching_problem))
        }

        # PREPARE INPUTS
        # * For vectorisation agents are referred to by their indice instead of
        #   their actual ids. Also note that, for efficiency, vectors should not
        #   resized when they have been created. Instead assigning NA or NULL values
        #   can improve speed significantly.
        chooser_pool <- 1:nrow(matching_problem$agentset_A)
        candidate_pool <- 1:nrow(matching_problem$agentset_B)

        # reduce the total matches to the length of the lesser pool
        if (length(chooser_pool) > length(candidate_pool)) {
          # BY DEFAULT, THE ORDER IS SHUFFLED
          chooser_pool <-
            sample(
              x = chooser_pool,
              size = length(candidate_pool),
              replace = FALSE
            )
        } else {
          # SHUFFLE ORDER
          chooser_pool <-
            sample(chooser_pool, length(chooser_pool))
        }

        matched_candidate_pool <-
          rep(NA, length(chooser_pool)) # for keeping match results

        # SELECT PICKING STRATEGY
        if (method == "pweighted") {
          picking_strategy <- function(choices, weights) {
            sample_choice(x = choices,
                          size = 1,
                          prob = weights)
          }
        }
        if (method == "ranking") {
          picking_strategy <- function(choices, weights) {
            choices[which.max(weights)]
          }
        }

        # MAIN MATCHING LOOP
        for (i in seq_along(chooser_pool)) {
          # get chooser index ---
          chooser <- chooser_pool[i]
          # randomly select n potential partners ---
          # adjust number of choices as the size of the candidate pool diminishes
          available_candidates <-
            candidate_pool[which(!is.na(candidate_pool))]
          n_available_candidates <- length(available_candidates)
          if (n_available_candidates < n_choices) {
            n_choices <- n_available_candidates
          }
          idx_of_potential_partner_idx <-
            sample_choice(x = available_candidates,
                          size = n_choices,
                          replace = FALSE)
          # A calculate matching scores towards chosen candidates from B pool ---
          scores <-
            self$matching_score_A(matching_problem = matching_problem,
                                  idx_A = chooser,
                                  idx_B = idx_of_potential_partner_idx)
          # weighted random draw ---
          matched_candidate_pool[i] <- partner_idx <-
            picking_strategy(choices = idx_of_potential_partner_idx,
                             weights = scores)
          # remove selected candidate from the pool ---
          candidate_pool[which(candidate_pool == partner_idx)] <-
            NA
        }
        # check all choosers found their matches
        checkmate::assert_integerish(
          matched_candidate_pool,
          any.missing = FALSE,
          null.ok = FALSE,
          unique = TRUE
        )

        # POST PROCESSING, id_A = 'chooser id' and id_B = 'candidate id'
        unmatched_candidates <-
          candidate_pool[!is.na(candidate_pool)]
        matches <- data.table(
          id_A = c(matching_problem$agentset_A[[self$matching_problem$id_col_A]][chooser_pool],
                   rep(NA, length(unmatched_candidates))),
          id_B = matching_problem$agentset_B[[self$matching_problem$id_col_B]][c(matched_candidate_pool, unmatched_candidates)])
      }
  )
)
