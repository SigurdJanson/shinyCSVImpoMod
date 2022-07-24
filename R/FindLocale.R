library(readr) # takes care of l10n issues

#'
.Locales <- list(
  `de-DE` = readr::locale(date_names = "de",
                 # dd.MM.yyyy; also allowed dd.MM.yy and yyyy.MM.dd
                 date_format   = c("%d.%m.%Y"), #, "%d.%m.%Y", "%Y-%m-%d"),
                 time_format   = c("%R"), #, "%T"), # "%H:%M", "%H:%M:%S"
                 decimal_mark  = ",",
                 grouping_mark = ".",
                 encoding = "UTF-8",
                 asciify = FALSE),
  `de-CH` = readr::locale(date_names = "de",
                 date_format   = c("%d.%m.%y"), #, "%d.%m.%Y", "%Y-%m-%d"),
                 time_format   = c("%R"), #, "%T"), # "%H:%M", "%H:%M:%S"
                 decimal_mark  = ".",
                 grouping_mark = "'",
                 encoding = "UTF-8",
                 asciify = FALSE),
  ko = readr::locale("ko"),
  en = readr::locale("en")
)


#' @title Find a locale specification for a given language code
#' @param LangCode A language code like "de-CH"  or "en-GB"
#' (i.e. in the form of a IETF language tag).
#' @details This function improves [readr::locale()]. It does not only allow a
#' language code but also supports a language specification.
#' @returns A locale object (see readr::locale())
#' @seealso [readr::locale()] where `locale` objects are specified
#' @references \url{https://en.wikipedia.org/wiki/IETF_language_tag},
#' \url{https://cran.r-project.org/web/packages/readr/vignettes/locales.html}
FindLocale <- function(LangCode) {
  if (missing(LangCode) || nchar(LangCode) == 0 || length(LangCode) == 0)
    LangCode <- "Default"
  Found <- which(LangCode == names(.Locales))
  if (length(Found) == 0) {
    Found <- startsWith(names(.Locales), LangCode)
    Found <- which(Found) # use first hit only
    if (length(Found) > 1) Found <- min(Found)
  }
  if (length(Found) == 0) {
    Info <- Sys.localeconv()
    FoundLocale <- readr::default_locale()
    # replace numeric separators because R usually formats numbers in the C locale
    FoundLocale[["decimal_mark"]]  <- unname(Info["mon_decimal_point"])
    FoundLocale[["grouping_mark"]] <- unname(Info["mon_thousands_sep"])
  } else {
    FoundLocale <- .Locales[[Found]]
  }
  return(FoundLocale)
}

