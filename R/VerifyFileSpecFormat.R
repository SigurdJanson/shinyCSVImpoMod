

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


VerifyFileSpecFormat <- function(Expected) {
  if (!isTruthy(Expected))
    return(.ExpectedDefault)

  if (!is.list(Expected))
    stop("Expected must be a list")

  return(FixupList(Expected, .ExpectedDefault))
}