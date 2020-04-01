#' @title Matching Market Class
#'
#' @description
#'
#' This is a the abstract base class for matching market objects like
#' [MatchingMarketStochastic] and [MatchingMarketOptimal]. This class is usually
#' used for solving problems such as allocation, matching and mating of agents.
#'
#' @usage NULL
#' @format [R6::R6Class] object.
#' @section Construction:
#' ```
#' matching_problem <- MatchingMarket$new(agentset_A, agentset_B, slots_B, id_col_A, id_col_B, grouping_vars)
#' ```
#'
#' * `agentset_A` :: [data.table::data.table()]\cr
#'   A data.table contains agentset A data and relavant attributes. This is usually
#'   the choosers (in one-sided matching).
#'
#' * `agentset_B` :: [data.table::data.table()]\cr
#'   A data.table contains agentset B data and relavant attributes. This is usually
#'   be the agents in the candidate pool.
#'
#' * `id_col_A` :: `character(1)`\cr
#'   A character that indicates the id column of `agentset_A`.
#'
#' * `id_col_B` :: `character(1)`\cr
#'   A character that indicates the id column of `agentset_B`.
#'
#' * `slots_B` :: `integer()`\cr
#'   (`NULL`)\cr
#'   A integer vector of length 1 or length equals to the number of rows in `agentset_B`.
#'   This is needed in `many-to-one` matching. Such as workers to jobs.
#'
#' * `grouping_vars` :: `character()`\cr
#'   (`NULL`)\cr
#'   When this is provided matching can be done just between agents of the same group.
#'
#' @section Fields:
#'
#'  * `matching_problem`\cr
#'
#' @section Methods:
#'
#'  * `split_market()`\cr
#'  (`logical(1)`) -> `()`\cr
#'  Create a list of list of data.tables. `list({group_name} = list({sub_market_number} = list(agentset_A = data.table(), agentset_B = data.table())))`.
#'
#'  * `split_by_group(matching_problem = self$matching_problem, grouping_vars = self$matching_problem$grouping_vars)`\cr
#'  (`agentset_A = data.table::data.table(), agentset_B = data.table::data.table()`|`logical()`, `character()`, `logical(1)`) -> `[data.table::data.table()]`\cr
#'  Create n-groups sub-matching problems.
#'
#'  * `split_by_n(matching_problem = self$matching_problem, n = NULL)`\cr
#'  (`agentset_A = data.table::data.table(), agentset_B = data.table::data.table()`| `integer()`) -> `list([data.table::data.table()])`
#'  Create n sub-matching problems. If n is not given, then n will be determine
#'  based on self$max_market_size.
#'
#'  * `matching_score_A(matching_problem = self$matching_problem, idx_A, idx_B)`\cr
#'  (`list(agentset_A = data.table::data.table(), agentset_B = data.table::data.table())`|`integer()`|`integer()` ) ->
#'  `integer()` of length `idx_B` or `matrix()` of length `idx_A` * length `idx_B`\cr
#'  This is an abstract function meaning it needs to be implemented. Please see
#'  the detail section for suggestion how this method should be implemented.
#'
#'  * `matching_score_B(matching_problem = self$matching_problem, idx_B, idx_A)`\cr
#'  (`list(agentset_A = data.table::data.table(), agentset_B = [data.table::data.table()])` |`integer()`|`integer()` ) ->
#'  `integer()` of length `idx_B` or `matrix()` of length `idx_A` * length `idx_B`\cr
#'  This is an abstract function meaning it needs to be implemented. Please see
#'  the detail section for suggestion how this method should be implemented.
#'
#' @details
#'  sub-matching problems only contains agentsets' data in a named list format
#'  (agentset_A, agentset_B).
#'
#' @export
MatchingMarket <- R6::R6Class(

# Public ------------------------------------------------------------------

  classname = "MatchingMarket",
  inherit = Generic,

  public = list(
    max_market_size = numeric(),
    matching_problem = list(
      agentset_A = data.table(),
      agentset_B = data.table(),
      slots_B = integer(),
      id_col_A = character(),
      id_col_B  = character(),
      grouping_vars = character()
    ),


    initialize = function(agentset_A,
                          agentset_B,
                          id_col_A,
                          id_col_B,
                          slots_B = NULL,
                          grouping_vars = NULL,
                          max_market_size = 5000 ^ 2) {
      # CHECK INPUTS
      checkmate::expect_data_table(agentset_A)
      checkmate::expect_data_table(agentset_B)

      if (missing(id_col_A)) {
        stopifnot(uniqueN(agentset_A[, 1]) == nrow(agentset_A))
        id_col_A <- names(agentset_A)[1]
      } else {
        checkmate::assert_names(names(agentset_A), must.include = id_col_A)
        if (uniqueN(agentset_A[[id_col_A]]) != nrow(agentset_A)) {
          stop("Please make sure the id column of agentset_A is correctly specified.")
        }
      }

      if (missing(id_col_B)) {
        stopifnot(uniqueN(agentset_B[[1]]) == nrow(agentset_B))
        id_col_B <- names(agentset_B)[1]
      } else {
        checkmate::assert_names(names(agentset_B), must.include = id_col_B)
        if (uniqueN(agentset_B[[id_col_B]]) != nrow(agentset_B)) {
          stop("Please make sure the id column of agentset_A is correctly specified.")
        }
      }

      if (!is.null(grouping_vars)) {
        checkmate::assert_names(names(agentset_A), must.include = grouping_vars)
        checkmate::assert_names(names(agentset_B), must.include = grouping_vars)
      }

      if (!is.null(slots_B)) {
        checkmate::assert_integerish(slots_B, len = nrow(agentset_B), lower = 1)
        agentset_B <- copy(agentset_B)[, slots := slots_B]
      }

      checkmate::assert_count(max_market_size, positive = T, na.ok = FALSE, null.ok = FALSE)

      if (max_market_size > 5000 ^ 2) {
        stop(
          glue::glue(
            "The maximum allowable market size is 5000^2. Anything more than this \\
            could be inefficient to compute on a laptop."
          )
        )
      }

      # STORE INPUTS
      # always make sure data.tables aren't stored as reference of the inputs
      # since there could be unwanted side-effects
      self$matching_problem$agentset_A <- copy(agentset_A)
      self$matching_problem$agentset_B <- copy(agentset_B)
      self$matching_problem$id_col_A <- id_col_A
      self$matching_problem$id_col_B <- id_col_B
      self$matching_problem$slots_B <- slots_B
      self$matching_problem$grouping_vars <- grouping_vars
      class(self$matching_problem) <- "matching_problem"
      self$max_market_size <- as.integer(max_market_size)

      # CHECK IMPLEMENTED METHODS
      private$.check_matching_score_fn()
      # private$.check_simulate_fn()

      return(invisible())
    },

    simulate = function() {
      private$abstract("Must be implemented based on one's use case.")
    },

    matching_score_A = function(matching_problem = self$matching_problem, idx_A, idx_B) {
      private$abstract("Must be implemented based on one's use case.")
    },

    matching_score_B = function(matching_problem = self$matching_problem, idx_B, idx_A) {
      private$abstract("Must be implemented based on one's use case.")
    },

    # create sub markets
    split_market = function(matching_problem = self$matching_problem, flatten = FALSE) {
      markets_by_group <- self$split_by_group()
      sub_markets <- purrr::map(.x = markets_by_group,
                                .f = ~ {
                                  self$split_by_n(matching_problem = .x)
                                })
      lg$info("The matching problem is splited into {length(markets_by_group)} groups (markets) \\
          with the total of {sum(sapply(sub_markets, length))} sub-markets")
      if (flatten) {
        return(purrr::flatten(sub_markets))
      } else {
        return(sub_markets)
      }
    },

    # Returns a list of data.tables
    split_by_group = function(matching_problem = self$matching_problem,
                              grouping_vars = self$matching_problem$grouping_vars) {
      if (is.null(grouping_vars)) {
        stop("`grouping_vars` was not given.")
      }
      if (!all(grouping_vars != self$matching_problem$grouping_vars)) {
        checkmate::assert_names(names(matching_problem$agentset_A), must.include = grouping_vars)
        checkmate::assert_names(names(matching_problem$agentset_B), must.include = grouping_vars)
      }
      # SPLIT DATA INTO GROUPS
      agentset_A_by_group <-
        split(matching_problem$agentset_A, by = grouping_vars, sorted = TRUE)
      agentset_B_by_group <-
        split(matching_problem$agentset_B, by = grouping_vars, sorted = TRUE)
      # CHECK VALIDITY
      # TODO: split_by_group should create an empty data.table for each missing group in either of the agentsets
      stopifnot(length(agentset_B_by_group) == length(agentset_A_by_group))
      stopifnot(all(names(agentset_A_by_group) == names(agentset_B_by_group)))
      matching_problem_by_group <-
        map2(.x = agentset_A_by_group,
             .y = agentset_B_by_group,
             .f = ~ {
               list(agentset_A = .x,
                    agentset_B = .y)
             })
      # RETURN
      return(matching_problem_by_group)
    },

    # Returns a list of data.tables
    split_by_n = function(matching_problem = self$matching_problem, n_submarkets = NULL) {
      checkmate::assert_count(n_submarkets, positive = T, null.ok = T, na.ok = FALSE)

      # * as.numeric is needed to avoid integer overflow
      # see https://stackoverflow.com/questions/8804779/what-is-integer-overflow-in-r-and-how-can-it-happen
      n_A <- nrow(matching_problem$agentset_A) %>% as.numeric()
      n_B <- nrow(matching_problem$agentset_B) %>% as.numeric()

      matching_problem_size <- n_A * n_B

      split_matching_problem <- function(n_A, n_B, n) {
        # self$msg_info("The matching problem is splited into {.n_submarkets} sub-markets", .n_submarkets = n_submarkets)
        # divide
        agentset_A_idx_list <-
          split(
            x = sample(1:n_A),
            f = seq_len(n_A) %% n_submarkets
          )

        agentset_B_idx_list <-
          split(
            x = sample(1:n_B),
            f = seq_len(n_B) %% n_submarkets
          )

        # construct sub-matching problems
        agentset_A_list <- lapply(agentset_A_idx_list, function(x) {
          matching_problem$agentset_A[x]
        })
        agentset_B_list <- lapply(agentset_B_idx_list, function(x) {
          matching_problem$agentset_B[x]
        })
        sub_matching_problem <-
          purrr::map2(
            .x = agentset_A_list,
            .y = agentset_B_list,
            .f = ~ {
              list(
                agentset_A = .x,
                agentset_B = .y
              )
          })
        return(sub_matching_problem)
      }

      if (!is.null(n_submarkets)) {
        return(split_matching_problem(n_A, n_B, n_submarkets))
      }

      determine_number_of_submarkets <- function(n_A, n_B) {
        current_market_size <- n_A * n_B
        number_of_submarkets <- ceiling(sqrt(current_market_size / self$max_market_size))
        return(number_of_submarkets)
      }

      if (matching_problem_size > self$max_market_size) {
        n_submarkets <- determine_number_of_submarkets(n_A, n_B)
        return(split_matching_problem(n_A, n_B, n_submarkets))
      } else {
        return(list(`0` = list(
          agentset_A = matching_problem$agentset_A,
          agentset_B = matching_problem$agentset_B
        )))
      }
      stop("something is wrong", browser())
    }
  ),

  private = list(

# Private -----------------------------------------------------------------

    .check_matching_score_fn = function() {
      if (!is(self, "MatchingMarket")) {
        lg$fatal("Must be implemented based on one's use case.")
        private$abstract()
      }
    },

    # expect a data.table as an input
    .check_final_match_result = function(match_result) {
      # if one-to-one/slots_B is null, then both columns id and id_B columns should not have
      # duplicates id

      # # if many-to-one/slots_B is not null, then both columns id should not have
      # duplicates id but id_B may have duplicates.
    },

    .early_return = function(matching_problem) {
      n_A <- nrow(matching_problem$agentset_A)
      n_B <- nrow(matching_problem$agentset_B)
      if (n_A == 0 & n_B == 0) {
        return(data.table(id_A = NA, id_B = NA))
      }
      if (n_A != 0 & n_B == 0) {
        return(data.table(id_A = matching_problem$agentset_A[[self$matching_problem$id_col_A]],
                          id_B = rep(NA, n_A)))
      }
      if (n_A == 0 & n_B != 0) {
        return(data.table(id_A = rep(NA, n_B),
                          id_B = matching_problem$agentset_B[[self$matching_problem$id_col_B]]))
      }
      stop("Early return conditions were not matched! something was wrong!")
    }
  )
)
