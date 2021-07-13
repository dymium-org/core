#' @title Pipeline
#'
#' @description
#'
#' Pipeline acts as a wraper for event functions. It can shuffle the order in which
#' the wrapped event functions get executed and also control how many times they
#' should be run within the pipeline. The later capability allows the modeller to
#' mix events with different time resolutions in a single microsimulation pipeline.
#' A more tangible example would be when you are modelling residential relocation
#' of households, renters may relocate every 6 months but buyers don't move that often
#' hence you can use the Pipeline to run a renter relocation event twice for every
#' run of a buy relocation event.
#'
#' ```{r eval = FALSE}
#' pipeline <- Pipeline$new(. %>%
#'                           renter_relocation_event())
#'
#' for (year in 1:5) {
#'   world %>%
#'     pipeline$run(n_loops = 2) %>%
#'     buyer_relocation_event()
#' }
#' ```
#'
#' @usage NULL
#' @format [R6::R6Class] object inheriting from [Generic].
#' @include Generic.R
#' @include Population.R
#'
#' @section Construction:
#'
#' ```
#' Pipeline$new(x)
#' ```
#'
#' * `x`\cr
#'   Event functions in a `%>%` structure where the start of the pipe must be `.`. \
#'   See the example section below.
#'
#' @section Public Methods:
#'
#' * `set(x)`\cr
#'   Sets the event functions inside the Pipeline object.
#'
#' * `get()`\cr
#'   Returns the event functions.
#'
#' * `run(x, shuffle = FALSE, n_loops = 1L)`\cr
#'  ([Container], `logical(1)`, `integer(1)`) -> [Container]\cr
#'   Execute the event functions in the order that it was added. To make the
#'   order randomised set shuffle as `TRUE`. `n_loops` controls how many times
#'   should the event functions be executed before exiting the Pipeline. If it is
#'   greater than one then the event functions will be run multiple times it
#'   the same order or randomised orders each time, depending on the value in `shuffle`.
#'
#' * `print()`\cr
#'
#' @export
#'
#' @examples
#'
#' # create Population
#' Pop <- Population$new(toy_individuals, toy_households, pid_col = "pid", hid_col = "hid")
#'
#' # create 3 dummy events
#' event1 <- function(object) {
#'   # do something
#'   return(object)
#' }
#'
#' event2 <- function(object) {
#'   # do something
#'   return(object)
#' }
#'
#' event3 <- function(object) {
#'   # do something
#'   return(object)
#' }
#'
#' # construct the sequence of the dummy events
#' # pipeline <- Pipeline$new(. %>% event1 %>% event2 %>% event3)
#' # pipeline$get()
#'
#' # run events in a random order
#' # pipeline$run(x = Pop, shuffle = TRUE)
Pipeline <- R6::R6Class(
  classname = "Pipeline",
  inherit = Generic,
  public = list(

    initialize = function(x) {
      if (!missing(x)) {
        self$set(x)
      }
    },

    set = function(x) {
      private$.events <- x
      self$print()
    },

    print = function() {
      flist <- magrittr::functions(private$.events)
      flistnames <- sapply(seq_along(flist), function(i) deparse(body(flist[[i]])))
      cat(glue::glue("There are {length(flist)} functions in the pipeline."), sep = "\n")
      cat(paste(seq_along(flistnames), flistnames, sep = ". "), sep = "\n")
    },

    get = function() {
      private$.events
    },

    run = function(x, shuffle = FALSE, n_loops = 1) {
      checkmate::assert(
        checkmate::check_r6(x, classes = c("Container", "Generic")),
        checkmate::check_logical(shuffle, any.missing = FALSE, len = 1, null.ok = FALSE),
        checkmate::check_number(n_loops, na.ok = FALSE, lower = 1, finite = TRUE, null.ok = FALSE),
        combine = "and"
      )

      if (length(private$.events) == 0L) {
        stop("The pipeline hasn't been set. Use the 'set' method to do so.")
      }

      # construct event order
      event_fseq <- private$.events
      if (shuffle) {
        event_order <- sample(seq_along(magrittr::functions(event_fseq)))
        event_fseq <- event_fseq[event_order]
      }

      event_flist_name <-
        sapply(seq_along(magrittr::functions(event_fseq)),
               function(x) deparse(body(event_fseq[[x]])))

      lg$info(paste(
        "EventManager is executing the events in this order: ",
        paste(paste("\t", seq_along(event_flist_name), ") ",
                    event_flist_name, sep = ""), collapse = "\n"),
        sep = "\n"
      ))

      for (i in 1:n_loops) {
        freduce(value = x, function_list = functions(event_fseq))
      }

      invisible(x)
    }
  ),

  private = list(
    .events = list()
  )
)
