clock_get <- function() {
  return(getOption("dymium.simulation_clock"))
}

clock_reset <- function() {
  clock_set(0)
}

clock_set <- function(x) {
  checkmate::assert_number(
    x,
    lower = 0,
    na.ok = FALSE,
    finite = T,
    null.ok = FALSE
  )
  options(dymium.simulation_clock = x)
  return(invisible())
}
