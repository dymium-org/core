#' @title Entity class
#'
#' @description
#'
#' The base class (first building block) for [Agent], [Asset] and [Environment].
#'
#' @usage NULL
#' @include Generic.R
#' @format [R6::R6Class] object.
#'
#' @section Construction:
#'
#' ```
#' x <- Entity$new(databackend, .data, id_col)
#' ```
#'
#' Stores `.data` as a DataBackend object inside the object's list of data (`private$.data`)
#' and registers the `id_col` (`private$.id_col`).
#'
#' * `databackend` :: an [R6::R6Class] generator]\cr
#'   An [R6::R6Class] generator that inherits from `DataBackend`.
#'
#' * `.data` :: `data.frame()`\cr
#'   A object that inherits from `data.frame`.
#'
#' * `id_col` :: `character()`\cr
#'   The name of the id column of `.data` and all relation columns. The first
#'   element will be checked as the main id column of the entity data, which
#'   must be unique integers. The rest of the vector will be consider as relation
#'   columns. For example, if `c("pid", "partner_id")` is given `pid` must contain
#'   unique integers, while `partner_id` can be `NA` or non-unique integers.
#'
#' @section Active Fields (read-only):
#'
#'  * `database`: a list of [DataBackend] objects that [Entity] possess.
#'
#'  * `id_col`: a character vector of all id columns with the first element being
#'   the main id column and the other elements, if any, are relation columns.
#'
#'  * `data_template`: a data.table object that contains the minimum data requirement apart
#'   from the `id_col`.
#'
#' @section Methods:
#'
#'  * `add_data(databackend = DataBackendDataTable, .data, name)`\cr
#'  (`DataBackend`, `data.frame`, `character(1)`) -> `NULL`\cr
#'  Append new data to the object's data list (`private$.data`). the new data
#'  must be linkable with the attribute data of the object by the id_col.
#'
#'  * `data(name)`\cr
#'  (`character(1)`) -> (`[dymiumCore::DataBackend]`|`NULL`)\cr
#'  Returns a [DataBackend] with the name that matches `name`.
#'  If `name` is not given, the function will try to return the [DataBackend] with name `attrs`.
#'  If `attrs` is not present or no `DataBackEnd` objects have been loaded it will
#'  return `NULL`.
#'
#'  * `add(.data, check_existing = FALSE)`\cr
#'  ([data.table::data.table()], `logical(1)`)\cr
#'  Add attribute data of new entities. This makes sure none of the ids
#'  of the new entities are the same as the existing entity records. However, other id columns,
#'  relation columns can be exempted from the check by setting `check_existing` as `FALSE`.
#'  Meaning, the other id columns can contain ids of the existing entities.
#'
#'  * `get_id_col`\cr
#'  () -> `character(1)`\cr
#'  Returns the column id field of data.
#'
#'  * `get_data(ids, copy = TRUE)`\cr
#'  (`integer()`, `logical(1)`) -> (`[data.table::data.table()]`|`NULL`)\cr
#'  Return agent data. When `ids` is specified, the data returned has the same
#'  row order as the order of `ids`. If `copy` is TRUE then data.table's
#'  reference semantics is returned. If `attrs` is not present or no `DataBackEnd`
#'  objects have been loaded it will return `NULL`.
#'
#'  * `get_data_names()`\cr
#'  () -> `character()`\cr
#'  Returns the names of all data objects (in `private$.data`) that belongs to this object.
#'
#'  * `get_removed_data(name)`\cr
#'  (`character(1)`) -> [data.table::data.table()]\cr
#'  Return removed agent data. If `name` is missing, the first data, which should
#'  contains the main attributes of the agent object, will be returned.
#'
#'  * `get_ids(include_removed = `FALSE`)`\cr
#'  (`logical(1)`) -> `integer()`\cr
#'  Return the ids of the indexes in the argrument `idx`, respectively. If `include_removed`
#'  is `TRUE`, ids of removed data will also be returned.
#'
#'  * `get_idx(ids)`\cr
#'  (`integer()`) -> `integer()`\cr
#'  Return the indexes of the ids in the argrument `ids`, respectively.
#'
#'  * `get_attr(x, ids)`\cr
#'  (`character(1)`, `integer()`) -> `vector(type::col)`\cr
#'  Extract the attribute from self$data as vector. If `ids` is given then only
#'  the corresponding values to `ids` are returned, in the same order as the `ids`.
#'
#'  * `has_attr(x)`\cr
#'  (`character()`) -> `logical()`\cr
#'  Check which of the attribute names given in `x` exist in the attribute data
#'  of the object.
#'
#'  * `ids_exist(ids, include_removed_data = FALSE)`\cr
#'  (`integer()`, `logical(1)`) -> `logical()`\cr
#'  Check whether `ids` exist or not.  And if `inclide_removed_data` is `TRUE` it
#'   will also check the removed data.
#'
#'  * `idx_exist(idx, by_element = FALSE)`\cr
#'  (`integer()`) -> `logical()`\cr
#'  Returns a logical vector of the same length as the argument `idx`. This function
#'  checks whether the indexes in the argument `idx` exist or not. If `by_element` is
#'  TRUE then it will return a logical vector with the same length as `ids` in
#'  their respective order.
#'
#'  * `n()`\cr
#'  () -> `integer(1)`\cr
#'  Returns the number of entities represented by this object.
#'
#'  * `print_data(n = 0)`\cr
#'  (`logical(1)`) -> `self`\cr
#'  Print to console the number of datasets and their dimensions. `n` is the number of rows
#'  that will be output to console by `head()`, if 0 nothing will be printed.
#'
#'  * `subset_ids(expr)`\cr
#'  (`expr`) -> `[data.table::data.table()]`\cr
#'  Return ids of rows matches the expression.
#'
#' @aliases Entities
#' @export
Entity <-
  R6::R6Class(
    classname = "Entity",
    inherit = Generic,
    public = list(
      initialize = function(databackend, .data, id_col) {
        checkmate::assert_character(id_col, null.ok = FALSE, min.len = 1, unique = T, any.missing = FALSE, names = "unnamed")
        checkmate::assert_names(names(.data), must.include = id_col, type = "strict")
        checkmate::assert_integerish(.data[[id_col[1]]], unique = TRUE, any.missing = FALSE, null.ok = FALSE, min.len = 1)
        private$.data[[1]] <- databackend$new(.data, key = id_col[1])
        checkmate::assert_r6(private$.data[[1]], classes = "DataBackend", .var.name = "databackend")
        names(private$.data)[1] <- "attrs"
        private$.last_id <- max(.data[[id_col[1]]])
        private$.id_col <- id_col
        invisible()
      },
      add_data = function(databackend = DataBackendDataTable, .data, name) {
        checkmate::assert_names(names(.data), must.include = private$.id_col[[1]], type = "strict")
        checkmate::assert_string(name, null.ok = FALSE, na.ok = FALSE)
        checkmate::assert_names(name, type = "strict")
        checkmate::assert_names(names(private$.data), disjunct.from = name)
        private$.data[[length(private$.data) + 1L]] <- databackend$new(.data)
        names(private$.data)[length(private$.data)] <- name
        invisible()
      },
      data = function(name) {
        if ((missing(name) & length(private$.data) == 0) | is.null(self$get_data_names())) {
          lg$warn("{class(self)[[1]]} has no data.")
          return(NULL)
        }

        if (missing(name)) {
          .data_pos <- 1
        } else {
          .data_pos <- which(names(private$.data) == name) # return an integer or integer(0) if no match
        }

        if (length(.data_pos) != 1) {
          stop(
            glue::glue(
              "name='{name}' didn't match any data in private$.data [{.data_names}].",
              .data_names = glue::glue_collapse(names(private$.data), ", ", last = " and ")
            )
          )
        }

        lg$trace("returning {names(private$.data)[[.data_pos]]}")
        return(private$.data[[.data_pos]])
      },
      get_data = function(name, ids, copy = TRUE) {
        if (missing(name)) {
          name <- "attrs"
        }

        DataObj <- self$data(name)

        if (is.null(DataObj)) {
          return(NULL)
        }

        if (copy == FALSE) {
          if (!missing(ids)) {
            stop("It is not possible to return a reference semetic to the specific rows in `ids`.")
          }
          return(DataObj$get(copy = FALSE))
        }

        if (missing(ids)) {
          return(DataObj$get())
        } else {
          checkmate::check_integerish(x = ids, unique = TRUE, lower = 1, min.len = 1, null.ok = FALSE, any.missing = FALSE)
          if (name == "attrs") {
            return(DataObj$get(rows = self$get_idx(ids)))
          } else {
            lg$warn("The order of the returned data is not garantee to be the same \\
                    with the input `ids`. Also not all ids are garantee to have \\
                    valid records.")
            return(DataObj$get()[get(self$get_id_col()) %in% ids, ])
          }
        }
      },
      get_data2 = function(name = "attrs", ids, copy = TRUE) {
        DataObj <- self$data(name)

        if (is.null(DataObj)) {
          return(NULL)
        }

        if (copy == FALSE) {
          if (!missing(ids)) {
            stop("It is not possible to return a reference semetic to the specific rows in `ids`.")
          }
          return(DataObj$get(copy = FALSE))
        }

        if (missing(ids)) {
          return(DataObj$get())
        } else {
          checkmate::check_integerish(x = ids, unique = TRUE, lower = 1, min.len = 1, null.ok = FALSE, any.missing = FALSE)
          if (name == "attrs") {
            if (is.null(DataObj$key)) {
              DataObj$setkey(self$get_id_col())
            }
            return(
              data.table:::na.omit.data.table(
                DataObj$get(copy = FALSE)[J(ids)],
                cols = DataObj$colnames[2]
              )
            )
          } else {
            lg$warn("The order of the returned data is not garantee to be the same \\
                    with the input `ids`. Also not all ids are garantee to have \\
                    valid records.")
            return(DataObj$get()[get(self$get_id_col()) %in% ids, ])
          }
        }
      },
      get_data_names = function() {
        names(private$.data)
      },

      # @param .data a data.frame or data.table object that contains data of new
      #  entities to be added. The new data must comply with the existing data fields
      #  of the existing entities' attribute data (`attrs`).
      # @check_existing :: `logical(1)`\cr
      #  Check the primary id of the new entities, in `.data`.
      #  Whether to check that all ids in id cols exist in the existing entity ids.
      #  If this function is to be called in a birth event, you probably want to
      #  set this to `TRUE` since a newborn individual agent would have its mother id
      #  of an existing individual agent. But if you are adding new individual agents
      #  to the existing individual data then you wouldn't expect that there should
      #  be existing ids
      add = function(.data, check_existing = FALSE) {
        checkmate::assert_data_frame(.data)
        checkmate::assert_flag(check_existing, na.ok = FALSE, null.ok = FALSE)

        # make sure the original copy of the data will not be mutated.
        .data <- data.table::copy(.data)

        # check data structure -----------
        NewData <- DataBackendDataTable$new(.data, key = self$primary_id)

        res <-
          all.equal(
            target = omit_derived_vars(self$database$attrs$data[0, ]),
            current = omit_derived_vars(NewData$data[0, ]),
            check.attributes = FALSE,
            ignore.col.order = TRUE
          )

        if (!isTRUE(res)) {
          cli::cli_alert_info("New data (.data)")
          print(NewData$head())
          cli::cli_alert_info("Existing data")
          print(self$database$attrs$head())
          stop(res)
        }

        # check id columns ----------
        checkmate::assert_integerish(
          .data[[self$primary_id]],
          any.missing = FALSE,
          null.ok = FALSE,
          unique = T
        )

        if (any(.data[[self$primary_id]] %in% self$get_ids(include_removed = T))) {
          lg$warn("entities in `.data` have the same ids as some of the existing \\
                  entities. The duplicated ids will be replaced.")
          data.table::set(
            x = .data,
            j = self$primary_id,
            value = self$generate_new_ids(n = .data[, .N])
          )
        }

        # check relation columns
        if (length(self$id_col) > 1) {
          ids_in_relation_cols <- c()
          relation_cols <-
            self$id_col[!self$id_col %in% self$primary_id]
          for (relation_col in relation_cols) {
            ids_in_relation_cols <-
              c(ids_in_relation_cols, na.omit(.data[[relation_col]]))
          }
          ids_in_relation_cols <- unique(ids_in_relation_cols)
          if (check_existing) {
            assert_subset2(ids_in_relation_cols, choices = c(self$get_ids(), .data[[self$primary_id]]))
          } else {
            assert_subset2(ids_in_relation_cols, choices = .data[[self$primary_id]])
          }
        }

        self$database$attrs$add(.data = .data, fill = TRUE)
        invisible()
      },
      has_attr = function(x) {
        x %in% self$database$attrs$colnames
      },
      get_attr = function(x, ids) {
        checkmate::assert_string(x, na.ok = FALSE, null.ok = FALSE)
        if (!missing(ids)) {
          return(self$get_data(copy = FALSE)[self$get_idx(ids = ids)][[x]])
        }
        self$data()$get(col = x)[[1]]
      },
      get_removed_data = function(name) {
        DataObj <- self$data(name)
        if (is.null(DataObj)) {
          return(NULL)
        } else {
          DataObj$get_removed()
        }
      },
      get_ids = function(include_removed = FALSE) {
        if (include_removed) {
          return(c(
            self$get_attr(self$primary_id),
            self$get_removed_data()[[self$primary_id]]
          ))
        }
        self$get_attr(self$primary_id)
      },
      get_idx = function(ids, expect_na = FALSE) {
        if (missing(ids)) {
          return(seq_len(self$data()$nrow()))
        }
        all_ids <- self$get_ids()
        if (expect_na == FALSE) {
          assert_entity_ids(self, ids)
        }
        sorted_idx <- which(all_ids %in% ids)
        sorted_ids <- all_ids[sorted_idx]
        tab <- data.table(id = sorted_ids, idx = sorted_idx)
        if (requireNamespace("fastmatch", quietly = TRUE)) {
          return(tab[fastmatch::fmatch(ids, id)][["idx"]])
        }
        tab[match(ids, id)][["idx"]]
      },
      get_id_col = function(all = FALSE) {
        if (all) {
          return(private$.id_col)
        } else {
          return(private$.id_col[[1]])
        }
      },
      remove = function(ids) {
        checkmate::assert_integerish(ids, any.missing = FALSE, unique = TRUE, lower = 1, min.len = 1)
        if (length(private$.data) == 0) {
          lg$warn("There is no data to be removed!")
          return(invisible())
        }
        for (DataObj in private$.data) {
          idx <- which(DataObj$get(copy = FALSE)[[private$.id_col[[1]]]] %in% ids)
          DataObj$remove(rows = idx)
        }
        invisible()
      },
      idx_exist = function(idx, by_element = FALSE) {
        checkmate::assert_integerish(x = idx, lower = 0, any.missing = FALSE, null.ok = FALSE)
        if (by_element) {
          return(self$data()$nrow() >= idx)
        } else {
          return(self$data()$nrow() >= max(idx))
        }
      },
      ids_exist = function(ids, include_removed_data = FALSE) {
        test_entity_ids(self, ids, include_removed_data = include_removed_data)
      },
      summary = function(verbose = TRUE) {
        if (length(private$.data) == 0) {
          summary_dt <-
            data.table(
              dataname = NA,
              ncol = NA,
              nrow = NA,
              nrow_removed = NA,
              size = NA
            )
        } else {
          summary_dt <-
            purrr::map2(
              .x = private$.data,
              .y = names(private$.data),
              .f = ~ {
                data.table(
                  dataname = .y,
                  ncol = .x$ncol(),
                  nrow = .x$nrow(),
                  nrow_removed = nrow(.x$get_removed()),
                  size = format(object.size(.x$get()), units = "Mb", standard = "SI")
                )
              }
            ) %>%
            rbindlist()
        }

        if (verbose) {
          print(knitr::kable(summary_dt))
        }

        invisible(summary_dt)
      },
      print = function() {
        .data_summary <- self$summary(verbose = FALSE) %>%
          .[, description := glue::glue("{dataname}[{nrow}, {ncol}]", .envir = .)]
        .class_inheritance <- glue::glue_collapse(class(self), sep = " <- ")
        .data_names <- glue::glue_collapse(.data_summary[["description"]], sep = ", ", last = " and ")
        .n_removed <- ifelse(is.null(self$get_removed_data()), 0, nrow(self$get_removed_data()))
        # if (requireNamespace('pryr', quietly = TRUE)) {
        #   .memory <- paste0(format(pryr::object_size(self) / 10^6, digits = 3), " MB")
        # } else {
        #   .memory <- "Not available, this requires the `pryr` package to be installed."
        # }

        message(
          glue::glue(
            "Class: {class(self)[[1]]}
               Inheritance: {.class_inheritance}
               Number_of_entities: {self$n()}
               Number_of_removed_entities: {.n_removed}
               Data[rows, cols]: {.data_names}
              "
          )
        )
      },
      print_data = function(n = 5) {
        if (n > 0) {
          print(purrr::map(private$.data, ~ .x$head(n)))
        }
        data_names <- glue::glue_collapse(names(private$.data), ", ", last = " and ")

        lg$info(
          glue::glue(
            "{class(self)[[1]]} has {length(private$.data)} datasets{seperator} {.data_names}",
            .data_names = ifelse(is.character(data_names), data_names, ""),
            seperator = ifelse(is.character(data_names), "...", "")
          )
        )
        invisible()
      },
      n = function() {
        if (is.null(self$data())) {
          return(0L)
        } else {
          self$data()$nrow()
        }
      },
      get_last_id = function() {
        private$.last_id
      },
      get_new_ids = function() {
        private$.new_ids
      },
      generate_new_ids = function(n) {
        checkmate::assert_integerish(n, lower = 1, len = 1, null.ok = FALSE, any.missing = FALSE)
        # generate new ids
        new_ids <- seq(
          from = self$get_last_id() + 1L,
          to = self$get_last_id() + n,
          by = 1L
        ) %>%
          as.integer()
        # update latest id
        private$.last_id <- private$.last_id + n
        # store the latest set of ids
        private$.new_ids <- new_ids
        # return the latest set of ids
        invisible(new_ids)
      },
      subset_ids = function(expr) {
        j_expr <- substitute(expr)
        subset(
          x = self$get_data(copy = FALSE),
          subset = eval(j_expr),
          select = self$get_id_col()
        )[[1]]
      }
    ),
    active = list(
      database = function() {
        get(".data", envir = private)
      },
      id_col = function() {
        get(".id_col", envir = private)
      },
      primary_id = function() {
        get(".id_col", envir = private)[[1]]
      },
      data_template = function() {
        return(data.table())
      }
    ),
    private = list(
      .data = list(),
      .id_col = NULL,
      .history = NULL,
      .last_id = NA_integer_,
      .new_ids = NA_integer_
    )
  )
