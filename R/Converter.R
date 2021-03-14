#
# shiny CSV import module
#
library(shiny)
library(readr) # takes care of l10n issues


#' @title Guess all column types of a data frame from character data
#' @param Data A data frame with all columns being of type `character`.
#' @param Locale Information about the underlying locale
#' @return A vector of strings indicating the data types
#' @seealso [readr::guess_parser()], [readr::locale()]
GuessColumnTypes <- function(Data, Locale = "de-DE") {
  if (!is.data.frame(Data)) stop("Invalid type of data")

  if (class(Locale) != "locale")
    if (is.character(Locale))
      Locale <- FindLocale(Locale)

  Types <- sapply(Data, readr::guess_parser, locale = Locale)
}


#' @title Convert columns of a data frame from character to their desired type
#' @param Columns A data frame with columns to cast types
#' @param Converter A list (see details)
#' @param Format A list of formats (see details)
#' @param Locale A locale that can be used in case format is missing
#' @details
#' Valid converters: date, time, datetime, character, factor, logical, number,
#' double, integer, find, regexfind. `NULL` drops a column. Everything else will be
#' guessed by [`readr::parse_guess()`].
#'
#' The types date, time, and datetime **support** a `Format` specification. If none is available
#' the `Locale` is used.
#'
#' The types "find" and "regexfind" **require** a `Format` specification.
#' @return a data frame with changed column data types
ColumnConvert <- function(Columns, Converter, Format, Locale) {
  if (!is.data.frame(Columns)) stop("Invalid type of 'Columns' data")
  if (!is.list(Converter)) stop("Invalid type of 'Converter' data")
  if (!missing(Format) && !is.null(Format) && !is.list(Format)) stop("Invalid type of 'Format' data")
  if (missing(Format)) Format <- NULL
  if (missing(Locale)) Locale <- default_locale()

  Col2Drop <- integer()

  for (i in 1:length(Columns)) {
    if (is.null(Converter[[i]])) {
      Col2Drop <- c(Col2Drop, i)
    } else {
      Format_i <- ifelse(isTruthy(Format[[i]]), Format[[i]], "")
      suppressWarnings({
        Columns[[i]] <- switch(
          Converter[[i]],
          date     = readr::parse_date(Columns[[i]], format = Format_i, locale = Locale),
          time     = readr::parse_time(Columns[[i]], format = Format_i, locale = Locale),
          datetime = readr::parse_datetime(Columns[[i]], format = Format_i, locale = Locale),
          character= readr::parse_character(Columns[[i]]), # no format required
          factor   = readr::parse_factor  (Columns[[i]]), # no format required
          logical  = readr::parse_logical (Columns[[i]], locale = Locale), # locale only
          number   = readr::parse_number  (Columns[[i]], locale = Locale), # locale only
          double   = readr::parse_double  (Columns[[i]], locale = Locale), # locale only
          integer  = readr::parse_integer (Columns[[i]], locale = Locale), # locale only
          find     = grepl(Format_i, Columns[[i]], fixed = TRUE),
          regexfind = grepl(Format_i, Columns[[i]], perl = TRUE),
          readr::parse_guess(Columns[[i]], locale = Locale)
        )
      })
    }
  }

  if (length(Col2Drop) > 0)
    for(i in length(Col2Drop):1) { # move backwards
      Columns[[ Col2Drop[i] ]] <- NULL
    }

  return(Columns)
}





#' @title Do conversions on a data frame
#' @param Df A data frame
#' @param ColSpec
#' @param Options
#' @param Preview FALSE (default) returns a working data frame. If `Preview` is `TRUE` or
#' numeric all columns are character. The number of rows can be changed with `Preview` being
#' numeric.
#' @importFrom ModuleImportServer
#' @return a data frame
DataFrameConvert <- function(Df, ColSpec, Options, Preview = FALSE) {
  if (missing(Df)) stop("Internal module error: data frame is missing")
  if (missing(ColSpec)) stop("Internal module error: column specification is missing")
  if (missing(Options)) stop("Internal module error: Locale options are missing")

  if (isTRUE(Preview) || is.numeric(Preview))
    Df <- head(Df, ifelse(is.numeric(Preview), Preview, 6L))

  # Match names to `ColSpec$NameInFile`, drop all missings, and
  # ... replace matching ones with desired names
  if (!is.null(ColSpec$NameInFile) && length(ColSpec$NameInFile) > 0L) {
    ColNames <- names(Df)
    # get logical vector identifying relevant positions
    WantedNFound <- intersect(ColSpec$NameInFile, ColNames) # Best case
    # Filter `Df` to remove un-requested columns
    Df <- Df[, ColNames %in% WantedNFound]

    # Reorder the requested columns to match the imported CSV
    # - get positions and use na.omit because otherwise, NA would still be in there
    Positions <- match(WantedNFound, ColSpec$NameInFile) #as.vector(na.omit(match(ColSpec$NameInFile, WantedNFound)))
    # - filter
    ColSpec$Name <- ColSpec$Name[Positions]
    ColSpec$NameInFile <- ColSpec$NameInFile[Positions] #FEHLER HIER
    ColSpec$Type <- ColSpec$Type[Positions]
    ColSpec$Format <- ColSpec$Format[Positions]

    # Set the correct names required by column specification
    names(Df) <- ColSpec$Name
  }

  # Get column types
  Locale <- locale(date_names    = PickTruthy(Options$LangCode, "en"),
                   date_format   = PickTruthy(Options$DateFormat, "%AD"),
                   time_format   = PickTruthy(Options$TimeFormat, "%AT"),
                   decimal_mark  = PickTruthy(Options$DecimalsSep, "."),
                   grouping_mark = PickTruthy(Options$ThousandsSep, ","),
                   tz = "UTC", encoding = "UTF-8", asciify = FALSE)
  ColTypes <- GuessColumnTypes(Df, Locale)
  if (isTruthy(ColSpec$Type)) { # pre-specified types take precedence over guessed type
    ColTypes <- replace(ColSpec$Type, !isTruthyInside(ColSpec$Type), ColTypes)
  }

  #
  if (isFALSE(Preview) && !is.numeric(Preview)) {
    Df <- ColumnConvert(Df, as.list(ColTypes), ColSpec$Format, Locale)
    if (!(isTruthy(ColSpec) && isTruthyInside(ColSpec))) {
      if (Options$StringsAsFactors) {
        Df[sapply(Df, is.character)] <- lapply(Df[sapply(Df, is.character)], as.factor)
      }
    }
  } else { # Preview
    # Create preview data frame
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
# ColSpec <- list(
#   Name       = as.list(c(paste0("Super", 1:3), "Extra", paste0("Super", 4:6))),
#   NameInFile = list("Spalte.A", "Name", "Alter", "Extra", "Datum", "GermanFloatingPoint", "Truth"),
#   Type       = list("character", "character", "integer", "time", "date", "number", "logical"),
#   Format     = list(NA, NA, NULL, NA, NA, NA, vector())
# )
# # df <- read.csv(system.file("inst", "extdata", "table.csv", package = "shiny.CSVImport"),
# #                header = TRUE, quote  = "", sep = ";", stringsAsFactors = FALSE,
# #                colClasses = "character")
# #
# df <- read.csv(header = TRUE, quote  = "", sep = ";", stringsAsFactors = FALSE,
#          colClasses = "character",
#          text = "Spalte A;Name;Alter;Datum;GermanFloatingPoint;Truth
# A;Jan;46;24.01.1975;1.024,64;TRUE
# B;Geert;49;19.04.1971;123,45;FALSE
# C;Hagen;41;03.02.1980;1.000.000,99;true")
# DataFrameConvert(df, ColSpec, .Options)



# df <- read.csv(system.file("inst", "extdata", "table.csv", package = "shiny.CSVImport"),
#                header = TRUE, quote  = "", sep = ";", stringsAsFactors = FALSE,
#                colClasses = "character")
# DataFrameConvert(df)

# df <- ColumnConvert(df, as.list(rep("logical", 6)))
# df <- ColumnConvert(df, as.list(rep("find", 5)), as.list(rep("^\\d*$", 5)))

