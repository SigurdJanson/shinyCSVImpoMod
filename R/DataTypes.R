


#' ColSpec2ShortType
#'
#' Converts a [vroom::cols] specification and turns it into standard abbreviations.
#'
#' @param colspec A [vroom::cols] specification
#'
#' @return a character vector with single characters
#' @export
#'
#' @examples
#' ColSpec2ShortType(vroom::cols(a = "i", b = "d", c = "T"))
#' #> integer  double datetime
#' #>     "i"     "d"      "T"
ColSpec2ShortType <- function(colspec) {
  if (!isTruthy(colspec)) return(NULL)

  .stopWord <- "collector"

  TypeStr <- sapply(colspec$cols, \(x) class(x)[class(x) != .stopWord])
  if (length(TypeStr) != length(colspec$cols)) stop("Unexpected format")
  TypeStr <- TypeStr |>
    strsplit("_") |>
    unlist() |>
    unname()
  TypeStr <- TypeStr[TypeStr != .stopWord]

  .ColumnDataTypesLong[TypeStr]
}





#' hasFormat
#'
#' @param ... A collection of objects or atomic vectors. All elements must
#' have the same type.
#' @details Basic `lists` are not supported, i.e.
#' `hasFormat(c(col_time(), col_double())` will not work.
#' @return A list of logicals. If the input contains vectors with length > 1 or
#' lists the returned list is nested.
#' @export
#'
#' @examples
#' hasFormat("t", "d", "i", "T")
#' hasFormat(c("t", "d", "i", "T"))
hasFormat <- function(...) {
  if (...length() == 0) return(FALSE)
  UseMethod("hasFormat")
}



#' @describeIn `hasFormat` Accepts character arguments: abbreviated (l = logical, i = integer, ...),
#' long format ("datetime", ...), or collector names ("col_double", "col_time", ...);
#' @details `hasFormat.character` accepts different string formats; but the must be consistent
#' to be interpreted properly (i.e. do not mix them as in `c("col_double", "d")`).
#' also see [vroom::cols].
#' @exportS3Method
hasFormat.character <- function(...) {
  Q <- list(...)

  stringType <- ifelse(
    all(rapply(Q, nchar) == 1), "short",
    ifelse(
      all(rapply(Q, startsWith, prefix="col_")), "collector",
      "long"))

  # Check if types are single letter abbreviations (i.e. "i" for integer, ...)
  # If not found any, check if types are full names (e.g. "col_integer")
  Type <- switch(
    stringType,
    short = lapply(Q, function(x) x[x %in% .ColumnTypes]),
    collector = lapply(Q, function(x) .ColumnTypes[match(x, names(.ColumnTypes), nomatch = NULL)]),
    long = lapply(Q, function(x) .ColumnTypesLong[match(x, names(.ColumnTypesLong), nomatch = NULL)]))

#browser()
  if (!all(rapply(Type, isTruthy))) stop("Some data types cannot be interpreted")

  ColSpec <- lapply(Type, function(x) do.call(cols, as.list(x)))

  # Format the result
  Result <- lapply(ColSpec, function(x) do.call(hasFormat.collector, x$cols))
  attributes(Result) <- NULL
  Result <- sapply(Result, `attributes<-`, NULL) # not needed if `ifelse` does that for us
  Result <- lapply(Result, unlist) #function(x) ifelse(length(x) == 0, unlist(x), x))
  return(Result)
}



#' @describeIn hasFormat Accepts lists of S3 class `collector` as generated
#' by \link[vroom:cols]{vroom::col*_()}.
#' @export
hasFormat.collector <- function(...) {
  lapply(list(...), function(x) any(names(x) == "format"))
}



#' @describeIn hasFormat Accepts column specification objects
#'
#' @export
hasFormat.col_spec <- function(...) {
  as.list(sapply(list(...), function(x) unlist(do.call(hasFormat.collector, x$cols))))
}



#' @describeIn hasFormat Accepts \link[vroom:cols]{vroom::col*_()} functions.
#' No nesting allowed (because it would create a list).
#'
#' @export
hasFormat.function <- function(...) {
  Args <- lapply(list(...), formalArgs)
  IsFormat <- lapply(Args, `==`, "format")
  return(lapply(IsFormat, any))
}


