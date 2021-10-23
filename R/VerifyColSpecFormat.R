
ColumnSpecificationListNames <- c("Name", "NameInFile", "Type", "Format")

lengthAllEqual <- function(x)
  sum(duplicated.default(sapply(x, length))) == (length(x)-1L)


#' VerifyColSpecFormat
#'
#' @param ColSpec A column specification. Either an object of class `col_spec`
#'
#' @return A column specification in the format of a `col_spec` class.
#' @seealso [readr::cols_condense()]
#' @examples
VerifyColSpecFormat <- function(ColSpec) {

  if (class(ColSpec) == "col_spec") {
    return(ColSpec)
  }

  if (class(ColSpec) == "tbl_df") {
    ColSpec <- vroom::spec(data.frame)
    return(ColSpec)
  }

  if (class(ColSpec) == "list") {
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

  stop("Unknown format of column specification")
}
