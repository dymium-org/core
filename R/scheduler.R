#' has the event been scheduled?
#'
#' @param time_steps a numeric vector
#'
#' @return a logical value
#' @export
is_scheduled <- function(time_steps) {
  if (!is.null(time_steps))
    stopifnot(all(is.numeric(time_steps)))
  if (is.null(time_steps))
    # time_steps is not specified
    return(TRUE)
  if (.get_sim_time() %in% time_steps)
    # the current time step matches a time in time_steps
    return(TRUE)
  else
    # doesn't match the current time step
    return(FALSE)
}
