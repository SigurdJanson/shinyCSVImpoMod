
ColumnSpecificationListNames <- c("Name", "NameInFile", "Type", "Format")

lengthAllEqual <- function(x)
  sum(duplicated.default(sapply(x, length))) == (length(x)-1L)


#' VerifyColSpecFormat
#'
#' @param ColSpec A column specification.
#' @return A valid column specification in the format of a `col_spec`
#' class that the import module can handle.
#' @seealso [readr::cols_condense()]
VerifyColSpecFormat <- function(ColSpec) {
  UseMethod("VerifyColSpecFormat")
}


#' @describeIn VerifyColSpecFormat Default method throws an error
#' to ignore unspecified data types.
VerifyColSpecFormat.default <- function(ColSpec)
  stop("Unknown format of column specification")


#' @describeIn VerifyColSpecFormat Handle `col_spec` objects and simply returns
#' the object itself.
VerifyColSpecFormat.col_spec <- function(ColSpec) {
  return(ColSpec)
}


#' @describeIn VerifyColSpecFormat Specific method to handle `tibble` objects.
VerifyColSpecFormat.tbl_df <- function(ColSpec) {
  ColSpec <- vroom::spec(ColSpec)
  return(ColSpec)
}

#' @describeIn VerifyColSpecFormat Specific method to handle `list`s.
#' It converts the information from the list into a valid `col_spec` object.
VerifyColSpecFormat.list <- function(ColSpec) {
  if(names(ColSpec) == ColumnSpecificationListNames) {
    # (Sub-) lists aren't of equal length
    if (!lengthAllEqual(ColSpec))
      stop("Specification suggest an ambiguous number of columns")

    # Make sure that `NameInFile` are syntactically valid and ...
    # that all missings are replaced by `Name`
    if (isTruthy(ColSpec$NameInFile)) {
      # NULL & NA is considered as missing
      Missing <- !isTruthyInside(ColSpec[["NameInFile"]])
      ColSpec$NameInFile[Missing] <- ColSpec$Name[Missing]
      ColSpec$NameInFile <- as.list(make.names(ColSpec$NameInFile))
    }

    # Convert from list spec
    .ColSpec <- ColSpec$Type
    names(.ColSpec) <- ColSpec$NameInFile
    ColSpec <- do.call(vroom::cols, .ColSpec)
    return(ColSpec)
  } else {
    stop("Wrong format of column specification")
  }
}
