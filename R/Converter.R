#
# shiny CSV import module
#
library(shiny)
library(readr)


#' @title Guess all column types of a data frame from character data
#' @param Data A data frame with all columns being of type `character`.
#' @param Locale Information about the underlying locale
#' @return A vector of strings indicating the data types
#' @seealso [readr::guess_parser()], [readr::locale()]
GuessColumnTypes <- function(Data, Locale = "de-DE") {
  if (!is.data.frame(Data)) stop("Invalid type of data")

  if (!inherits(Locale, "locale"))
    if (is.character(Locale))
      Locale <- FindLocale(Locale)

  Types <- sapply(Data, readr::guess_parser, locale = Locale)
}


#' @title ColumnConvert
#' @description
#' Convert columns of a data frame to their desired types. Columns in
#' source data frame are all of type `character`.
#'
#' @param Data A data frame with columns to cast types
#' @param Converter A list (see details)
#' @param Format A list of formats (see details)
#' @param Locale A locale that can be used in case format is missing
#' @details
#' Valid converters: date, time, datetime, character, factor, logical, number,
#' double, integer. Long and short names are allowed (see [`vroom::cols()`]).
#'
#' `NULL` drops a column. Everything else will be  guessed by [`readr::parse_guess()`].
#'
#' The types date, time, and datetime **support** a `Format` specification.
#' If none is available the `Locale` is used.
#' @return A data frame with changed column data types
ColumnConvert <- function(Data, Converter, Format, Locale) {
  if (!is.data.frame(Data))
    stop("Invalid type of 'Columns' data")
  if (!is.list(Converter))
    stop("Invalid type of 'Converter' data")
  if (!missing(Format) && !is.null(Format) && !is.list(Format))
    stop("Invalid type of 'Format' data")

  if (missing(Format)) Format <- NULL
  if (missing(Locale)) Locale <- default_locale()

  # Parse formats to get unambiguous strings
  Converter[sapply(Converter, is.null)] <- "skip" # replace NULL with string
  .Cnvrt <- vroom::as.col_spec(Converter) # return object of class `col_spec`
  .Cnvrt <- sapply(.Cnvrt$cols, function(i) class(i)[[1]])
  .Cnvrt <- substr(.Cnvrt, nchar("collector_")+1, 99) # remove leading "collector_"

  Col2Drop <- integer()

  for (i in 1:length(Data)) { # use length instead of `ncol` because of performance
    if (.Cnvrt[i] == "skip") {
      Col2Drop <- c(Col2Drop, i)
    } else {
      Format_i <- ifelse(isTruthy(Format[[i]]), Format[[i]], "")
      suppressWarnings({
        Data[[i]] <- switch(
          .Cnvrt[i],
          date     = readr::parse_date(Data[[i]], format = Format_i, locale = Locale),
          time     = readr::parse_time(Data[[i]], format = Format_i, locale = Locale),
          datetime = readr::parse_datetime(Data[[i]], format = Format_i, locale = Locale),
          character= readr::parse_character(Data[[i]]), # no format required
          factor   = readr::parse_factor  (Data[[i]]),  # no format required
          logical  = readr::parse_logical (Data[[i]], locale = Locale), # locale only
          number   = readr::parse_number  (Data[[i]], locale = Locale), # locale only
          double   = readr::parse_double  (Data[[i]], locale = Locale), # locale only
          integer  = readr::parse_integer (Data[[i]], locale = Locale), # locale only
          readr::parse_guess(Data[[i]], locale = Locale)
        )
      })
    }
  }

  # Remove skipped columns
  if (length(Col2Drop) > 0L)
    for(i in length(Col2Drop):1) { # move backwards
      Data[[ Col2Drop[i] ]] <- NULL
    }

  return(Data)
}





#' @title Change the type of data frame columns.
#' @param Df A data frame
#' @param Preview FALSE (default) returns a working data frame. If `Preview` is `TRUE` or
#' numeric all columns are character. The number of rows can be changed with `Preview` being
#' numeric.
#' @inheritParams ModuleImportServer
#' @return A data frame
DataFrameConvert <- function(Df, ColSpec, FileSpec, Preview = FALSE) {
  if (missing(Df) || !isTruthy(Df))
    stop("Internal module error: data frame is missing")
  if (missing(ColSpec) || !isTruthy(ColSpec))
    stop("Internal module error: column specification is missing")
  if (missing(FileSpec) || !isTruthy(FileSpec))
    stop("Internal module error: Locale options are missing")
  if (isTruthy(ColSpec$NameInFile) && !all(isTruthyInside(ColSpec$NameInFile)))
    stop("Internal module error: 'NameInFile' must be specified")


  if (isTRUE(Preview) || is.numeric(Preview))
    Df <- head(Df, ifelse(is.numeric(Preview), Preview, 6L))


  # Match names to `ColSpec$NameInFile`, drop all missings, and
  # ... replace matching ones with desired names
  if (!is.null(ColSpec$NameInFile) && length(ColSpec$NameInFile) > 0L) {
    ColNames <- names(Df)
    # get logical vector identifying relevant positions
    WantedNFound <- intersect(ColNames, ColSpec$NameInFile) # order of arguments counts!
    # Filter `Df` to remove un-requested columns
    Df <- Df[, ColNames %in% WantedNFound]
    if (ncol(Df) == 0L) stop("msgConfigurationYieldsEmpty")

    # Reorder the requested columns to match the imported CSV
    # - get positions and use na.omit because otherwise, NA would still be in there
    Positions <- match(WantedNFound, ColSpec$NameInFile) #as.vector(na.omit(match(ColSpec$NameInFile, WantedNFound)))
    # - filter
    ColSpec$Name <- ColSpec$Name[Positions]
    ColSpec$NameInFile <- ColSpec$NameInFile[Positions] #TODO: FEHLER HIER
    ColSpec$Type <- ColSpec$Type[Positions]
    ColSpec$Format <- ColSpec$Format[Positions]

    # Set the correct names required by column specification
    names(Df) <- ColSpec$Name
  }

  # Get column types
  Locale <- locale(date_names    = PickTruthy(FileSpec$LangCode, "en"),
                   date_format   = PickTruthy(FileSpec$DateFormat, "%AD"),
                   time_format   = PickTruthy(FileSpec$TimeFormat, "%AT"),
                   decimal_mark  = PickTruthy(FileSpec$DecimalsSep, "."),
                   grouping_mark = PickTruthy(FileSpec$ThousandsSep, ","),
                   tz = "UTC", encoding = "UTF-8", asciify = FALSE)
  ColTypes <- GuessColumnTypes(Df, Locale)
  # Pre-specified types take precedence over guessed type
  if (isTruthy(ColSpec$Type)) {
    ColTypes <- replace(ColSpec$Type, !isTruthyInside(ColSpec$Type), ColTypes)
  }

  #
  if (isFALSE(Preview) && !is.numeric(Preview)) {
    Df <- ColumnConvert(Df, as.list(ColTypes), ColSpec$Format, Locale)

    if (FileSpec$StringsAsFactors)
      CoerceToFactor <- sapply(Df, is.character)
    else if (isTruthy(ColSpec) && all(isTruthyInside(ColSpec)))
      CoerceToFactor <- ColSpec$Type == "factor"
    else
      CoerceToFactor <- logical()

    if (length(CoerceToFactor) > 0L)
      for (i in 1:ncol(Df))
        if (CoerceToFactor[i]) Df[[i]] <- as.factor(Df[[i]])

  } else { # Preview
    # Create preview data frame
    if (isTRUE(FileSpec$StringsAsFactors)) {
      ColTypes <- sapply(ColTypes, USE.NAMES = FALSE,
                         FUN = function(x) ifelse(x == "character", "factor", x))
    }
    ColTypesStr <- sprintf("<%s>", ColTypes)
    Df <- rbind(Types = ColTypesStr, Df)
  }

  return(Df)
}


# .Options <- list(
#   LangCode = "en",
#   Header = TRUE,
#   ColSep = ";",
#   ThousandsSep = ".",
#   DecimalsSep = ",",
#   DateFormat = "%d.%m.%Y",
#   TimeFormat = "%H:%M:%S", # strptime() default
#   Quote = "",
#   StringsAsFactors = FALSE
# )
# df <- read.csv(header = TRUE, quote  = "", sep = ";", stringsAsFactors = FALSE,
#          colClasses = "character",
#          text = "Spalte A;Name;Alter;Datum;GermanFloatingPoint;Truth
# A;Jan;46;24.01.1975;1.024,64;TRUE
# B;Geert;49;19.04.1971;123,45;FALSE
# C;Hagen;41;03.02.1980;1.000.000,99;true")


# ColSpec = list(
#   Name = list("Name", "Age", "Date", "Double", "T/F", "Time", "LETTER"),
#   NameInFile = list("Name", "Age", "Datum", "GermanFloatingPoint", "Truth", "Time", "Column.A"),
#   Type = list("character", "integer", "date", "number", "logical", "time", "character"),
#   Format = list(NA, NA, "%d.%m.%Y", NA, NA, "%H:%M", NA)
# )
# DataFrameConvert(df, ColSpec, .Options)

