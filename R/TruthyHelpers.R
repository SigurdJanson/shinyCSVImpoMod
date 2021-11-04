
#' @title isTruthyInside
#' @description `isTruthyInside` tests the truthiness of positions
#' of a vector (while `isTruthy` tests the vector as a whole).
#' @param x A vector or list
#' @return A logical vector for each position indicating `TRUE`/`FALSE`.
#' Returns `NULL` if operation was not successful.
#' @export
#' @examples
#' isTruthyInside(1:5)
#' #> [1] TRUE TRUE TRUE TRUE TRUE
#' isTruthyInside(c(1, NA, 2))
#' #> [1]  TRUE FALSE  TRUE
#' isTruthyInside(list(1, NA, NULL, integer(0), 2))
#' #> [1]  TRUE FALSE FALSE FALSE  TRUE
isTruthyInside <- function(x) {
  Result <- sapply(x, isTruthy, USE.NAMES = FALSE)
  if (!isTruthy(Result) || length(Result) == 0) return(NULL)
  return(Result)
}



#' @title PickTruthy
#' @description Choose a value if it truthy. If not, choose it's replacement
#' @param X A value of any type
#' @param Replacement The replacement in case `X` is falsy
#' @return `X` or `Replacement`
#' @export
#' @examples
#' PickTruthy(1, 2) # 1
#' PickTruthy("1", 2) # "1"
#' PickTruthy("", 2) # 2
#' PickTruthy(NULL, 3) # 3
#' PickTruthy(NULL, NA) # NA
PickTruthy <- function(X, Replacement) {
  return( ifelse(isTruthy(X), X, Replacement) )
}

