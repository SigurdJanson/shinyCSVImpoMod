#
# shiny CSV import module
#
require(readr) # takes care of l10n issues


GuessColumnTypes <- function(Data, Locale = "de-DE") {
  if (!is.data.frame(Data)) stop("Invalid type of data")

  if (class(Locale) != "locale")
    if (is.character(Locale))
      Locale <- FindLocale(Locale)

  Types <- sapply(Data, readr::guess_parser, locale = Locale)
}


#' @param list of character vectors (incl a data frame)
#' @details
#' Valid converters: character, logical, numeric, double, integer, factor, datetime, regex,
ColumnConvert <- function(Columns, Converter, Format) {

}

ColumnConvert2Date <- function() {

}

