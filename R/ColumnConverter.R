#
# shiny CSV import module
#
require(readr) # takes care of l10n issues


#' @title Guess all column types of a data frame from character data
#' @param Data A data frame with all columns being of type `character`.
#' @param Locale Information about the underlying locale
#' @return A vector of strings indicating the data types
#' @seealso [readr::guess_parser()], [readr::locale()]
#' @examples
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
#'
#' @details
#' Valid converters: character, logical, numeric, double, integer, factor, datetime, find,
ColumnConvert <- function(Columns, Converter, Format) {
  if (!is.data.frame(Columns)) stop("Invalid type of 'Columns' data")
  if (!is.list(Converter)) stop("Invalid type of 'Converter' data")
  if (!missing(Format) && !is.list(Format)) stop("Invalid type of 'Format' data")

  Col2Drop <- integer()

  for (i in 1:length(Columns)) {
    if (is.null(Converter[[i]])) {
      Col2Drop <- c(Col2Drop, i)
    } else {
      suppressWarnings({
        Columns[[i]] <- switch(
          Converter[[i]],
          date     = readr::parse_date(Columns[[i]], format = Format[[i]]),
          time     = readr::parse_time(Columns[[i]], format = Format[[i]]),
          datetime = readr::parse_datetime(Columns[[i]], format = Format[[i]]),
          factor   = readr::parse_factor  (Columns[[i]]), # no format required
          logical  = readr::parse_logical (Columns[[i]]), # locale only
          number   = readr::parse_number  (Columns[[i]]), # locale only
          double   = readr::parse_double  (Columns[[i]]), # locale only
          integer  = readr::parse_integer (Columns[[i]]), # locale only
          find     = grepl(Format[[i]], Columns[[i]], fixed = TRUE),
          regexfind = grepl(Format[[i]], Columns[[i]]),
          readr::parse_guess(Columns[[i]])
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

# df <- read.csv(system.file("inst", "extdata", "table.csv", package = "shiny.CSVImport"),
#                header = TRUE, quote  = "", sep = ";", stringsAsFactors = FALSE,
#                colClasses = "character")
# df <- ColumnConvert(df, as.list(rep("logical", 6)))
# df <- ColumnConvert(df, as.list(rep("find", 5)), as.list(rep("^\\d*$", 5)))
