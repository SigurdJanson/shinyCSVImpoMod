

#' PeekIntoFile
#' Investigates the format of a delimiter separated file.
#' @param File The name of the file to investigate
#' @param GetSpec Shall the function return a `col_spec` object or
#' the data?
#' @param ... Additional arguments handed over to [`vroom::vroom`].
#'
#' @return Depending on `GetSpec` the column specification (which is an extended
#' object of class `col_spec` with additional fields),
#' the data frame (as returned from [vroom]).
PeekIntoFile <- function(File, GetSpec = FALSE, ...) {
  if (!isTruthy(File))
    stop("Invalid file name given. Cannot peek into invalid file.")

  Args <- c(file=File, list(...), show_col_types = FALSE)

  Data <- do.call(vroom::vroom, Args)
  Problems <- vroom::problems(Data)

  if (nrow(Problems) > 0) {
    attr(Problems, "problems") <- TRUE
    return(Problems)
  }
  else if (isTRUE(GetSpec)) {
    FileSpec <- vroom::spec(Data)
    FileSpec$HasHeader <- HasFileHeader(FileSpec, File, ...)
    return(FileSpec)
  }
  else
    return(Data)
}


#' HasFileHeader
#'
#' @param HeaderFileSpec A specification read from a CSV file with `col_names = TRUE`, i.e.
#' with the assumption that the file has a header.
#' @param File The according file that made the data set of `Data`.
#' @param ... Additional arguments handed over to [`vroom::vroom`].
#' @return TRUE/FALSE
HasFileHeader <- function(HeaderFileSpec, File, ...) {
  .CompareType <- function(x, y) isa(x, class(y))

  stopifnot(isa(HeaderFileSpec, "col_spec"))

  Args <- c(file=File, list(...), col_names = FALSE, show_col_types = FALSE)
  NoHeaderData <-  do.call(vroom::vroom, Args)
  NoHeaderSpec <- vroom::spec(NoHeaderData)

  #-Result <- !all(sapply(Data$cols, class) == sapply(NoHeaderData$cols, class))

  TypeMatches <- mapply(.CompareType, HeaderFileSpec$cols, NoHeaderSpec$cols)
  #-print(TypeMatches)
  if (anyNA(TypeMatches))
    return(TRUE)
  else
    return(any(TypeMatches == FALSE))
}

