

#' PeekIntoFile
#' Investigates the format of a delimiter separated file.
#' @param File The name of the file to investigate
#' @param GetSpec Shall the function return a `col_spec` object or
#' the data?
#' @param ... Additional arguments handed over to [`vroom::vroom`].
#'
#' @return Depending on `GetSpec` the column specification or the
#' data frame (as returned from [vroom]).
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
  else if (isTRUE(GetSpec))
    return(vroom::spec(Data))
  else
    return(Data)
}
