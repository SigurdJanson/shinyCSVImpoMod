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
