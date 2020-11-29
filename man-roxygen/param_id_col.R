#' @param id_col (`character()`)\cr
#'   ID fields in `.data`. The name of the id column of `.data` and all relation columns.
#'   The first element will be checked as the main id column of the entity data, which
#'   must be unique integers. The rest of the vector will be consider as relation
#'   columns. For example, if `c("pid", "partner_id")` is given `pid` must contain
#'   unique integers, while `partner_id` can be `NA` or non-unique integers.
