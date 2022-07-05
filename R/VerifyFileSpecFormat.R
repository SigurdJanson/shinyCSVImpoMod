

.FileSpecDefault <- list(
  LangCode = "en",
  Header = TRUE,
  ColSep = " ",
  ThousandsSep = ",",
  DecimalsSep = ".",
  DateFormat = "%Y-%m-%d", # strptime() default
  TimeFormat = "%H:%M:%S", # strptime() default
  Quote = "",
  StringsAsFactors = FALSE
)


VerifyFileSpecFormat <- function(FileSpec, Required = FALSE) {
  if (!isTruthy(FileSpec)) {
    if (Required)
      stop("File specification is required but missing or invalid")
    else
      return(.FileSpecDefault)
  }

  if (!is.list(FileSpec))
    stop("Expected must be a list")

  return(FixupList(FileSpec, .FileSpecDefault))
}
