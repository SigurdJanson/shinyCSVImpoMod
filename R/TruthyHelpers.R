

#' @title isTruthyInside tests the truthiness of positions of a vector (while `isTruthy`
#' tests the vector as a whole)
#' @param x A vector or list
#' @return A logical vector for each position indicating `TRUE`/`FALSE`
#' @examples
#' isTruthyInside(1:5)
#' #> [1] TRUE TRUE TRUE TRUE TRUE
#' isTruthyInside(c(1, NA, 2))
#' #> [1]  TRUE FALSE  TRUE
#' isTruthyInside(list(1, NA, NULL, integer(0), 2))
#' #> [1]  TRUE FALSE FALSE FALSE  TRUE
isTruthyInside <- function(x) {
  Result <- sapply(x, isTruthy)
  if (!isTruthy(Result) || length(Result) == 0) return(NULL)
  return(Result)
}


