

.ExpectedDefault <- list(
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


VerifyExpectedFormat <- function(Expected) {
  if (!is.null(Expected)) {
    if(anyNA(match(names(Expected), names(.ExpectedDefault))))
      stop("Invalid specification of expected format: unknown fields in data structure")
  } else {
    Expected <- .ExpectedDefault
  }

  return(Expected)
}
