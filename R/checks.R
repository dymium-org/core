.checkNames <- function(x, allow_numbers = FALSE, allow_underscores = FALSE) {
  stopifnot(!missing(x))

  .pattern <- fcase(
    isTRUE(allow_numbers) & isTRUE(allow_underscores), "^[a-zA-Z0-9_]+$",
    isTRUE(allow_numbers) & isFALSE(allow_underscores), "^[a-zA-Z0-9]+$",
    isFALSE(allow_numbers) & isTRUE(allow_underscores), "^[a-zA-Z_]+$",
    isFALSE(allow_numbers) & isFALSE(allow_underscores), "^[a-zA-Z]+$"
  )

  if (all(grepl(pattern = .pattern, x))) {
    return(TRUE)
  }

  # error message
  msg <- fcase(
    isTRUE(allow_numbers) &
      isTRUE(allow_underscores), "Name should only contain letters, numbers and underscores.",
    isTRUE(allow_numbers) &
      isFALSE(allow_underscores), "Name should only contain letters and numbers.",
    isFALSE(allow_numbers) &
      isTRUE(allow_underscores), "Name should only contain letters and underscores.",
    isFALSE(allow_numbers) &
      isFALSE(allow_underscores), "Name should only contain letters."
  )

  msg
}
