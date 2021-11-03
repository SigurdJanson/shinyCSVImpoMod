#' Convert characters to utf8 representation
#' @param x A string
#' @return All characters are replaced by unicode representations
.HandleUTF8 <- function(x){
  map <- function(x) {
    m <- utf8ToInt(x)
    #-if (is.na(m)) x <- enc2utf8(x)
    return(ifelse(is.na(m), x, sprintf("&#%d;", m)))
  }
  xs <- strsplit(as.character(x), "")[[1]]
  paste0(sapply(xs, map), collapse="")
}



#' FixupList
#' @description Checks a list for falsy or missing values and replaces them by
#' values taken from a default.
#' Remove extraneous elements in x that aren't part of the default.
#' Add missing elements in x.
#' Replace falsy values in x by default values.
#' @param x A named list to fix
#' @param Default A named list of default values
#'
#' @return The fixed list
#' @examples
#' # FixupList(list(a=1, b=NA, c=3, d=NA), list(a=4, d=8, c=9, x=7))
#' #> list(a=1, c=3, d=8, x=7)
FixupList <- function(x, Default) {
  stopifnot(!(is.null(x) || is.null(Default)))

  Falsies <- !isTruthyInside(x)

  # Remove extra values in x
  x <- x[names(x) %in% names(Default)]
  # Replace falsy values in x[..] by Default[..]
  Result <- replace(x,
                    Falsies,
                    Default[match(names(x), names(Default))][Falsies])
  # Add missings in x
  Result <- c(Result, Default[!(names(Default) %in% names(x))])

  return(Result)
}
