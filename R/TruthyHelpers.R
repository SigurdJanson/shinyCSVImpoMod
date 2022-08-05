
#' @title isTruthyInside
#' @description `isTruthyInside` tests the "truthiness" of positions
#' of a vector or list (while `isTruthy` tests the vector as a whole).
#' @param x A vector or list
#' @return A logical vector for each position indicating `TRUE`/`FALSE`.
#' Returns `NULL` if the object `x` itself is not truthy or missing.
#' @details `isTruthyInside`
#' @export
#' @examples
#' isTruthyInside(1:5)
#' #> [1] TRUE TRUE TRUE TRUE TRUE
#' isTruthyInside(c(1, NA, 2))
#' #> [1]  TRUE FALSE  TRUE
#' isTruthyInside(list(1, NA, NULL, integer(0), 2))
#' #> [1]  TRUE FALSE FALSE FALSE  TRUE
isTruthyInside <- function(x) {
  if (missing(x)) return(NULL)
  if (!isTruthy(x)) return(NULL) # that is not on the inside

  Result <- sapply(x, isTruthy, USE.NAMES = FALSE)
    return(unname(Result))
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



# https://github.com/rstudio/shiny/issues/2641
# `isTruthy` test the object and not it's content. I know why you ask that. I needed it, too, and I had to create my own function. However, changing `isTruthy` puts some overhead on every simple case when we just have to check: "is the object (as a whole) truthy?" Once we start looking deeper, how deep shall we go?
#
# `data.frame(NA)` shall yield `FALSE`. What about `data.frame(c(1, NA, 3))`? In essence, once we start looking into nested data structures we'd have to consider *all* the complexity. Would we not?
