

#' PeekIntoFile
#' Investigates the format of a delimiter separated file.
#' @param File A file to investigate
#' @param GetSpec Shall the function return a `col_spec` object or
#' the data?
#' @param ... Additional arguments handed over to [`vroom::vroom`].
#'
#' @return Depending on `GetSpec` the column specifition or the
#' data frame (as returned from [vroom]).
PeekIntoFile <- function(File, GetSpec = FALSE, ...) {
  if (!isTruthy(File))
    stop("Cannot peek into invalid file")

  dots <- list(...)

  Data <- do.call(vroom::vroom(File, dots))
  Problems <- vroom::problems(Data)

  if (nrow(Problems) > 0)
    return(Problems)
  else
    return(ifelse(GetSpec, vroom::spec(Data), Data))
}
